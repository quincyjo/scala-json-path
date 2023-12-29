/*
 * Copyright 2023 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.quincyjo.jsonpath.parser

import cats.implicits._
import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.parser.JsonPathParseContext.JsonPathToken
import com.quincyjo.jsonpath.parser.models._
import com.quincyjo.jsonpath.{JsonPath, parser}

import scala.annotation.tailrec
import scala.collection.mutable

object JsonPathParser {

  type Builder = mutable.Builder[JsonPathNode, List[JsonPathNode]]

  /** Parse the input string into a [[JsonPath]] if valid, or a [[ParseError]]
    * if not.
    * @return
    *   A [[ParseResult]] of a [[JsonPath]] from the input string.
    */
  def parse(input: String): ParseResult[JsonPath] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        builder: Builder
    ): ParseResult[JsonPathParseContext] = {
      if (!context.hasNext) Parsed(context)
      else
        parseNext(context, builder) match {
          case error: ParseError => error
          case Parsed(context)   => go(context, builder)
        }
    }

    for {
      x <- parseRoot(JsonPathParseContext(input))
      (maybeRoot, newContext) = x
      builder = List.newBuilder[JsonPathNode]
      _ <- go(newContext, builder)
      path = builder.result()
    } yield JsonPath(maybeRoot, path)
  }

  def take(input: String): ParseResult[ValueAt[JsonPath]] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        builder: Builder
    ): ParseResult[JsonPathParseContext] = {
      if (!context.hasNext) Parsed(context)
      else
        parseNext(context, builder) match {
          case _: ParseError   => Parsed(context)
          case Parsed(context) => go(context, builder)
        }
    }

    for {
      x <- parseRoot(JsonPathParseContext(input))
      (maybeRoot, newContext) = x
      builder = List.newBuilder[JsonPathNode]
      finalContext <- go(newContext, builder)
      path = builder.result()
      finalIndex = finalContext.nextIndex.getOrElse(input.length)
    } yield ValueAt(JsonPath(maybeRoot, path), 0, input.take(finalIndex))
  }

  private def parseRoot(
      context: JsonPathParseContext
  ): ParseResult[(Option[JsonPathRoot], JsonPathParseContext)] =
    context.peek
      .flatMap {
        case Some(JsonPathToken.Root) if context.index == 0 =>
          Parsed(Some(Root) -> context.nextToken())
        case Some(JsonPathToken.Current) if context.index == 0 =>
          Parsed(Some(Current) -> context.nextToken())
        case _ => Parsed(None -> context)
      }

  private def parseNext(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] = {
    val newContext = context.nextToken()
    newContext.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.RecursiveDescent =>
        parseRecursiveDescent(newContext, builder)
      case JsonPathToken.StartSelector | JsonPathToken.DotSelector =>
        parseProperty(newContext, builder)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          newContext.index,
          newContext.input,
          JsonPathToken.RecursiveDescent,
          JsonPathToken.StartSelector,
          JsonPathToken.DotSelector
        )
    }
  }

  def parseRecursiveDescent(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] = {
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.RecursiveDescent =>
          context.peek.flatMap { nextToken =>
            nextToken
              .fold[ParseResult[JsonPathParseContext]] {
                builder.addOne(RecursiveDescent())
                Parsed(context)
              } {
                case JsonPathToken.Wildcard | JsonPathToken.ValueInt |
                    JsonPathToken.ValueString =>
                  parseDotSelectorChild(context).map {
                    case (context, selector) =>
                      builder.addOne(RecursiveDescent(selector))
                      context
                  }
                case JsonPathToken.StartSelector =>
                  parseSelector(context).map { case (context, selector) =>
                    builder.addOne(RecursiveDescent(selector))
                    context
                  }
                case _ =>
                  builder.addOne(RecursiveDescent())
                  Parsed(context)
              }
          }
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            context.index,
            context.input,
            JsonPathToken.RecursiveDescent
          )
      }
  }

  def parseProperty(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    parseSelector(context).map { case (context, selector) =>
      builder.addOne(Property(selector))
      context
    }

  def parseSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.StartSelector => parseBracketSelector(context)
        case JsonPathToken.DotSelector   => parseDotSelectorChild(context)
        case invalidToken =>
          ParseError
            .invalidToken(
              invalidToken,
              context.index,
              context.input,
              JsonPathToken.DotSelector,
              JsonPathToken.StartSelector
            )
      }

  private def parseUnion(
      first: SingleSelector,
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Union)] = {

    def go(
        context: JsonPathParseContext,
        acc: mutable.Builder[SingleSelector, Seq[SingleSelector]]
    ): ParseResult[
      (
          JsonPathParseContext,
          mutable.Builder[SingleSelector, Seq[SingleSelector]]
      )
    ] = {
      val c1 = context.nextToken()
      for {
        nextSelector <- c1.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.ValueString =>
            c1.valueAsString.map(string => Attribute(string.value))
          case JsonPathToken.ValueInt =>
            c1.valueAsNumber.map(number => Index(number.value))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              c1.index,
              c1.input,
              JsonPathToken.ValueString,
              JsonPathToken.ValueInt
            )
        }
        c2 = c1.nextToken()
        result <- c2.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.EndSelector =>
            Parsed(c2 -> acc.addOne(nextSelector))
          case JsonPathToken.Union => go(c2, acc.addOne(nextSelector))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              c2.index,
              c2.input,
              JsonPathToken.Union,
              JsonPathToken.EndSelector
            )
        }
      } yield result
    }

    go(context, Seq.newBuilder[SingleSelector])
      .map { case (context, builder) =>
        context -> builder.result()
      }
      .flatMap {
        case (context, second :: tail) =>
          Parsed(context -> Union(first, second, tail))
        case _ =>
          ParseError(
            s"Trailing '${JsonPathToken.Union}' token at index ${context.index}.",
            context.index,
            context.input
          )
      }
  }

  private def parseSlice(
      context: JsonPathParseContext,
      start: Option[Int]
  ): ParseResult[(JsonPathParseContext, Slice)] = {

    def go(
        context: JsonPathParseContext,
        builder: mutable.Builder[Option[Int], Seq[Option[Int]]]
    ): ParseResult[
      (
          parser.JsonPathParseContext,
          mutable.Builder[Option[Int], Seq[Option[Int]]]
      )
    ] = {
      val newContext = context.nextToken()
      newContext.currentTokenOrEndOfInput.flatMap {
        case JsonPathToken.EndSelector => Parsed(newContext -> builder)
        case JsonPathToken.Slice | JsonPathToken.ValueInt
            if builder.knownSize > 3 =>
          ParseError(
            s"Too many slice arguments.",
            newContext.index,
            newContext.input
          )
        case JsonPathToken.Slice => go(newContext, builder.addOne(None))
        case JsonPathToken.ValueInt =>
          val c2 = newContext.nextToken()
          (
            newContext.valueAsNumber.map(_.value),
            c2.currentTokenOrEndOfInput
          ).mapN { case (value, token) =>
            builder.addOne(Some(value))
            token match {
              case JsonPathToken.EndSelector => Parsed(c2 -> builder)
              case JsonPathToken.Slice       => go(c2, builder)
              case invalidToken =>
                ParseError.invalidToken(
                  invalidToken,
                  newContext.index,
                  newContext.input,
                  JsonPathToken.ValueInt,
                  JsonPathToken.Slice,
                  JsonPathToken.EndSelector
                )
            }
          }.flatten
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            newContext.index,
            newContext.input,
            JsonPathToken.ValueInt,
            JsonPathToken.Slice,
            JsonPathToken.EndSelector
          )
      }
    }

    go(context, Seq.newBuilder[Option[Int]].addOne(start))
      .map { case (context, builder) =>
        context -> builder.result()
      }
      .flatMap { case (context, parts) =>
        Slice(
          parts.headOption.flatten,
          parts.lift(1).flatten,
          parts.lift(2).flatten
        )
          .map(slice => Parsed(context -> slice))
          .getOrElse(
            ParseError(
              "At least one slice parameter is required.",
              context.index,
              context.input
            )
          )
      }
  }

  def parseSlice(
      context: JsonPathParseContext,
      start: Int
  ): ParseResult[(JsonPathParseContext, Slice)] =
    parseSlice(context, Some(start))

  private def parseBracketSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] = {
    val c1 = context.nextToken()
    for {
      firstToken <- c1.currentTokenOrEndOfInput
      maybeFirstSelector <- Option(firstToken).collect {
        case JsonPathToken.ValueString =>
          c1.valueAsString.map(v => Attribute(v.value))
        case JsonPathToken.ValueInt =>
          c1.valueAsNumber.map(v => Index(v.value))
        case JsonPathToken.Wildcard => Parsed(Wildcard)
        case JsonPathToken.StartExpression =>
          c1.valueAsExpression.map(v => Script(v.value))
        case JsonPathToken.StartFilterExpression =>
          c1.valueAsExpression.map(v => Filter(v.value))
      }.sequence
      c2 = if (maybeFirstSelector.isDefined) c1.nextToken() else c1
      secondToken <- Option
        .when(maybeFirstSelector.isDefined)(c2)
        .traverse(_.currentTokenOrEndOfInput)
      selector <- (maybeFirstSelector, firstToken, secondToken) match {
        case (Some(firstSelector), _, Some(JsonPathToken.EndSelector)) =>
          Parsed(c2 -> firstSelector)
        case (
              Some(singleSelector: SingleSelector),
              _,
              Some(JsonPathToken.Union)
            ) =>
          parseUnion(singleSelector, c2)
        case (Some(Index(index)), _, Some(JsonPathToken.Slice)) =>
          parseSlice(c2, index)
        case (None, JsonPathToken.Slice, None) =>
          parseSlice(c2, None)
        case (_, invalidToken, _) =>
          ParseError.invalidToken(
            invalidToken,
            c2.index,
            c2.input,
            JsonPathToken.EndSelector,
            JsonPathToken.Union,
            JsonPathToken.Slice
          )
      }
    } yield selector
  }

  private def parseDotSelectorChild(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] = {
    val nextContext = context.nextToken()
    nextContext.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.Wildcard => Parsed(nextContext -> Wildcard)
      case JsonPathToken.ValueString =>
        nextContext.valueAsString
          .map(string => nextContext -> Attribute(string.value))
      case JsonPathToken.ValueInt =>
        nextContext.valueAsNumber
          .map(number => nextContext -> Index(number.value))
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          nextContext.index,
          nextContext.input,
          JsonPathToken.Wildcard,
          JsonPathToken.ValueString,
          JsonPathToken.ValueInt
        )
    }
  }
}
