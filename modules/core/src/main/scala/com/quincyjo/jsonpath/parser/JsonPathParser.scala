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

  private type Builder = mutable.Builder[JsonPathNode, List[JsonPathNode]]

  /** Parse the input string into a [[JsonPath]] if valid, or a
    * [[models.ParseError]] if not.
    * @return
    *   A [[models.ParseResult]] of a [[JsonPath]] from the input string.
    */
  def parse(input: String): ParseResult[JsonPath] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        builder: Builder
    ): ParseResult[JsonPathParseContext] = {
      if (!context.hasNext) Parsed(context)
      else
        parseNode(context.nextToken(), builder) match {
          case error: ParseError => error
          case Parsed(context)   => go(context, builder)
        }
    }

    for {
      x <- parseRoot(JsonPathParseContext(input))
      (maybeRoot, newContext) = x
      builder = List.newBuilder[JsonPathNode]
      _ <- go(newContext, builder)
    } yield JsonPath(maybeRoot, builder.result())
  }

  /** Parse the input string into a [[JsonPath]] up to the first error.
    * @return
    *   A [[models.ParseResult]] of a [[JsonPath]] from the input string.
    */
  def take(input: String): ParseResult[ValueAt[JsonPath]] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        builder: Builder
    ): ParseResult[JsonPathParseContext] = {
      if (!context.hasNext) Parsed(context)
      else
        parseNode(context.nextToken(), builder) match {
          case _: ParseError   => Parsed(context)
          case Parsed(context) => go(context, builder)
        }
    }

    for {
      x <- parseRoot(JsonPathParseContext(input))
      (maybeRoot, newContext) = x
      builder = List.newBuilder[JsonPathNode]
      finalContext <- go(newContext, builder)
    } yield ValueAt(
      JsonPath(maybeRoot, builder.result()),
      0,
      input.take(finalContext.nextIndex.getOrElse(input.length))
    )
  }

  private def parseRoot(
      context: JsonPathParseContext
  ): ParseResult[(Option[JsonPathRoot], JsonPathParseContext)] =
    context.peek
      .collect {
        case JsonPathToken.Root    => Root
        case JsonPathToken.Current => Current
      }
      .fold(Option.empty[JsonPathRoot] -> context) { root =>
        Some(root) -> context.nextToken()
      }

  private def parseNode(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.RecursiveDescent =>
        parseRecursiveDescent(context, builder)
      case JsonPathToken.StartSelector | JsonPathToken.DotSelector =>
        parseProperty(context, builder)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          context.index,
          context.input,
          JsonPathToken.RecursiveDescent,
          JsonPathToken.StartSelector,
          JsonPathToken.DotSelector
        )
    }

  private def parseRecursiveDescent(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.RecursiveDescent =>
          context.peek.foldF {
            builder.addOne(RecursiveDescent())
            Parsed(context)
          } {
            case JsonPathToken.Wildcard | JsonPathToken.ValueInt |
                JsonPathToken.ValueString =>
              parseDotSelector(context.nextToken()).map {
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
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            context.index,
            context.input,
            JsonPathToken.RecursiveDescent
          )
      }

  private def parseProperty(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    parseSelector(context).map { case (context, selector) =>
      builder.addOne(Property(selector))
      context
    }

  private def parseSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.StartSelector =>
          parseBracketSelector(context.nextToken())
        case JsonPathToken.DotSelector =>
          parseDotSelector(context.nextToken())
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

  private def parseBracketSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.Slice =>
        parseSlice(context, None)
      case JsonPathToken.StartExpression |
          JsonPathToken.StartFilterExpression =>
        parseExpression(context)
      case _ =>
        parseDotSelector(context).flatMap { case (context, selector) =>
          val nextContext = context.nextToken()
          nextContext.currentTokenOrEndOfInput.flatMap {
            case JsonPathToken.Slice =>
              selector match {
                case Index(value) =>
                  parseSlice(nextContext, Some(value))
                case invalidSelector =>
                  ParseError(
                    s"Slice requires an index but was $invalidSelector",
                    nextContext.index,
                    nextContext.input
                  )
              }
            case JsonPathToken.EndSelector =>
              Parsed(context.nextToken() -> selector)
            case JsonPathToken.Union =>
              parseUnion(selector, nextContext)
            case invalidToken =>
              ParseError.invalidToken(
                invalidToken,
                nextContext.index,
                nextContext.input,
                JsonPathToken.EndSelector,
                JsonPathToken.Union,
                JsonPathToken.Slice
              )
          }
        }
    }

  private def parseDotSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, SingleSelector)] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.Wildcard => Parsed(context -> Wildcard)
      case JsonPathToken.ValueString =>
        context.valueAsString
          .map(string => context -> Attribute(string.value))
      case JsonPathToken.ValueInt =>
        context.valueAsNumber
          .map(number => context -> Index(number.value))
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          context.index,
          context.input,
          JsonPathToken.Wildcard,
          JsonPathToken.ValueString,
          JsonPathToken.ValueInt
        )
    }

  private def parseExpression(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, ScriptSelector)] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.StartExpression =>
          context.valueAsExpression.map(v => Script(v.value))
        case JsonPathToken.StartFilterExpression =>
          context.valueAsExpression.map(v => Filter(v.value))
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            context.index,
            context.input,
            JsonPathToken.StartExpression,
            JsonPathToken.StartFilterExpression
          )
      }
      .flatMap { selector =>
        val nextContext = context.nextToken()
        nextContext.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.EndSelector =>
            Parsed(nextContext -> selector)
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              nextContext.index,
              nextContext.input
            )
        }
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
    ] =
      for {
        nextSelector <- context.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.ValueString =>
            context.valueAsString.map(string => Attribute(string.value))
          case JsonPathToken.ValueInt =>
            context.valueAsNumber.map(number => Index(number.value))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              context.index,
              context.input,
              JsonPathToken.ValueString,
              JsonPathToken.ValueInt
            )
        }
        nextContext = context.nextToken()
        result <- nextContext.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.EndSelector =>
            Parsed(nextContext -> acc.addOne(nextSelector))
          case JsonPathToken.Union =>
            go(nextContext.nextToken(), acc.addOne(nextSelector))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              nextContext.index,
              nextContext.input,
              JsonPathToken.Union,
              JsonPathToken.EndSelector
            )
        }
      } yield result

    go(context.nextToken(), Seq.newBuilder[SingleSelector])
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
    ] =
      context.currentTokenOrEndOfInput.flatMap {
        case JsonPathToken.EndSelector => Parsed(context -> builder)
        case JsonPathToken.Slice | JsonPathToken.ValueInt
            if builder.knownSize > 3 =>
          ParseError(
            s"Too many slice arguments.",
            context.index,
            context.input
          )
        case JsonPathToken.Slice =>
          go(context.nextToken(), builder.addOne(None))
        case JsonPathToken.ValueInt =>
          val nextContext = context.nextToken()
          (
            context.valueAsNumber.map(_.value),
            nextContext.currentTokenOrEndOfInput
          ).mapN { case (value, token) =>
            builder.addOne(Some(value))
            token match {
              case JsonPathToken.EndSelector => Parsed(nextContext -> builder)
              case JsonPathToken.Slice       => go(nextContext.nextToken(), builder)
              case invalidToken =>
                ParseError.invalidToken(
                  invalidToken,
                  context.index,
                  context.input,
                  JsonPathToken.ValueInt,
                  JsonPathToken.Slice,
                  JsonPathToken.EndSelector
                )
            }
          }.flatten
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            context.index,
            context.input,
            JsonPathToken.ValueInt,
            JsonPathToken.Slice,
            JsonPathToken.EndSelector
          )
      }

    go(context.nextToken(), Seq.newBuilder[Option[Int]].addOne(start))
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
}
