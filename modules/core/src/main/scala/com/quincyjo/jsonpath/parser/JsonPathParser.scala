/*
 * Copyright 2023 Quincy Jo
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
import com.quincyjo.jsonpath.extensions.{Extension, StandardExtensions}
import com.quincyjo.jsonpath.parser.models.JsonPathParseContext.JsonPathToken
import com.quincyjo.jsonpath.parser.models._
import com.quincyjo.jsonpath.{Expression, JsonPath}

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.mutable

/** A parser for [[com.quincyjo.jsonpath.JsonPath]] s.
  *
  * Example usage to parse an entire string:
  * {{{
  * scala> JsonPathParser.parse("$.foo.bar.baz")
  * val res0: ParseResult[JsonPath] = Parsed($.foo.bar.baz)
  * }}}
  * Example usage to parse a string up to the first error:
  * {{{
  * JsonPathParser.take("$.foo is a JSON path.")
  * scala> JsonPathParser.parse("$.foo is a JSON path.")
  * val res0: ParseResult[ValueAt[JsonPath]] = Parsed(ValueAt($.foo,0,$.foo ))
  * }}}
  */
trait JsonPathParser {

  private type Builder = mutable.Builder[JsonPathSegment, List[JsonPathSegment]]

  protected val enableArithmeticOperators: Boolean = false

  /** Parse the input string into a [[JsonPath]] if valid, or a
    * [[models.ParseError]] if not.
    * @return
    *   A [[models.ParseResult]] of a [[JsonPath]] from the input string.
    */
  def parse(input: String): ParseResult[JsonPath] =
    parse(input, failThrough = true).map { case (_, path) =>
      path
    }

  /** Parse the input string into a [[JsonPath]] up to the first error.
    * @return
    *   A [[models.ParseResult]] of a [[JsonPath]] from the input string.
    */
  def take(input: String): ParseResult[ValueAt[JsonPath]] =
    parse(input, failThrough = false).map { case (context, path) =>
      ValueAt(path, 0, context.nextIndex.fold(input)(input.take))
    }

  protected val extensions = new AtomicReference[
    List[Extension[?, ?]]
  ](List.empty)

  private lazy val expressionParser: ExpressionParser =
    ExpressionParser(
      extensions.get,
      this,
      enableArithmeticOperators = enableArithmeticOperators
    )

  private def parse(
      input: String,
      failThrough: Boolean
  ): ParseResult[(JsonPathParseContext, JsonPath)] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        builder: Builder
    ): ParseResult[JsonPathParseContext] = {
      if (!context.hasNext) Parsed(context)
      else
        parseSegment(context.nextToken(), builder) match {
          case error: ParseError => if (failThrough) error else Parsed(context)
          case Parsed(context)   => go(context, builder)
        }
    }

    for {
      x <- parseRoot(JsonPathParseContext(input, expressionParser))
      (root, newContext) = x
      builder = List.newBuilder[JsonPathSegment]
      finalContext <- go(newContext, builder)
    } yield finalContext -> JsonPath(root, builder.result())
  }

  private def parseRoot(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathRoot, JsonPathParseContext)] =
    context.peek
      .collect {
        case JsonPathToken.Root    => Root
        case JsonPathToken.Current => Current
      }
      .foldF[(JsonPathRoot, JsonPathParseContext)](
        ParseError(
          "A JSON Path must start with either '$' or '@'",
          context.index,
          context.input
        )
      ) { root =>
        Parsed(root -> context.nextToken())
      }

  private def parseSegment(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.RecursiveDescent =>
        parseDescendantSegment(context.nextToken(), builder)
      case JsonPathToken.StartSelector | JsonPathToken.DotSelector =>
        parseChildSegment(context, builder)
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

  private def parseDescendantSegment(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.Wildcard | JsonPathToken.ValueInt |
          JsonPathToken.ValueString => // TODO: Should quotes be allowed here?
        parseShorthandSelector(context).map { selector =>
          builder.addOne(RecursiveDescent(selector))
          context
        }
      case JsonPathToken.StartSelector =>
        parseBracketSelector(context).map { case (context, selector) =>
          builder.addOne(RecursiveDescent(selector))
          context
        }
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          context.index,
          context.input,
          JsonPathToken.Wildcard,
          JsonPathToken.ValueInt,
          JsonPathToken.ValueString,
          JsonPathToken.StartSelector
        )
    }

  private def parseChildSegment(
      context: JsonPathParseContext,
      builder: Builder
  ): ParseResult[JsonPathParseContext] =
    parseSelector(context).map { case (context, selector) =>
      builder.addOne(JsonPathSegment(selector))
      context
    }

  private def parseSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case JsonPathToken.StartSelector =>
          parseBracketSelector(context)
        case JsonPathToken.DotSelector =>
          val nextContext = context.nextToken()
          parseShorthandSelector(nextContext).map(nextContext -> _)
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
    parseAnySelector(context.nextToken())
      .flatMap { case (context, selector) =>
        context.currentTokenOrEndOfInput.flatMap {
          case JsonPathToken.EndSelector => Parsed(context -> selector)
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              context.index,
              context.input,
              JsonPathToken.EndSelector
            )
        }
      }

  private def parseShorthandSelector(
      context: JsonPathParseContext
  ): ParseResult[Selector] =
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.Wildcard =>
        Parsed(Wildcard)
      case JsonPathToken.ValueString
          if context.input.lift(context.index).exists(_.isLetterOrDigit) =>
        context.valueAsString
          .map(string => Attribute(string.value))
      case JsonPathToken.ValueInt =>
        context.valueAsNumber
          .map(number => Index(number.value))
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

  private def parseAnySelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Selector)] = {
    parseComposableSelector(context).flatMap { case (context, selector) =>
      context.currentTokenOrEndOfInput.flatMap {
        case JsonPathToken.EndSelector => Parsed(context -> selector)
        case JsonPathToken.Union =>
          parseUnion(selector, context)
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            context.index,
            context.input,
            JsonPathToken.EndSelector,
            JsonPathToken.Union
          )
      }
    }
  }

  private def parseComposableSelector(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, ComposableSelector)] = {
    context.currentTokenOrEndOfInput.flatMap {
      case JsonPathToken.ValueString =>
        context.valueAsString
          .map(string => context.nextToken() -> Attribute(string.value))
      case JsonPathToken.ValueInt =>
        context.valueAsNumber
          .map(number => context.nextToken() -> Index(number.value))
          .flatMap { case (context, index) =>
            context.currentTokenOrEndOfInput.flatMap {
              case JsonPathToken.Slice => parseSlice(context, Some(index.value))
              case _                   => Parsed(context -> index)
            }
          }
      case JsonPathToken.Wildcard =>
        Parsed(context.nextToken() -> Wildcard)
      case JsonPathToken.Slice =>
        parseSlice(context, None)
      case JsonPathToken.StartFilterExpression =>
        parseExpression(context)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          context.index,
          context.input,
          JsonPathToken.ValueString,
          JsonPathToken.ValueInt,
          JsonPathToken.Wildcard,
          JsonPathToken.Slice,
          JsonPathToken.StartFilterExpression
        )
    }
  }

  private def parseExpression(
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Filter)] =
    context.valueAsExpression.map(_.value).flatMap {
      case logical: Expression.LogicalType =>
        Parsed(Filter(logical))
      case nodes: Expression.NodesType =>
        Parsed(Filter(nodes))
      case other =>
        ParseError(
          s"Filter requires a logical expression but was: $other",
          context.index,
          context.input
        )
    } match {
      case Parsed(value) => Parsed(context.nextToken() -> value)
      case ParseError(message, index, _, cause) =>
        ParseError(message, context.index + index, context.input, cause)
    }

  private def parseUnion(
      first: ComposableSelector,
      context: JsonPathParseContext
  ): ParseResult[(JsonPathParseContext, Union)] = {

    @tailrec
    def go(
        context: JsonPathParseContext,
        acc: mutable.Builder[ComposableSelector, Seq[ComposableSelector]]
    ): ParseResult[
      (
          JsonPathParseContext,
          mutable.Builder[ComposableSelector, Seq[ComposableSelector]]
      )
    ] =
      parseComposableSelector(context).flatMap { case (context, selector) =>
        context.currentTokenOrEndOfInput.map { token =>
          (selector, token, context)
        }
      } match {
        case parseError: ParseError => parseError
        case Parsed((selector, token, context)) =>
          acc.addOne(selector)
          if (token == JsonPathToken.EndSelector)
            Parsed(context -> acc)
          else if (token != JsonPathToken.Union)
            ParseError.invalidToken(
              token,
              context.index,
              context.input,
              JsonPathToken.Union
            )
          else go(context.nextToken(), acc)
      }

    go(context.nextToken(), Seq.newBuilder[ComposableSelector])
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
          JsonPathParseContext,
          mutable.Builder[Option[Int], Seq[Option[Int]]]
      )
    ] =
      context.currentTokenOrEndOfInput.flatMap {
        case JsonPathToken.EndSelector | JsonPathToken.Union =>
          Parsed(context -> builder)
        case JsonPathToken.Slice | JsonPathToken.ValueInt
            if builder.knownSize > 2 =>
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
              case JsonPathToken.EndSelector | JsonPathToken.Union =>
                Parsed(nextContext -> builder)
              case JsonPathToken.Slice => go(nextContext.nextToken(), builder)
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
            JsonPathToken.EndSelector,
            JsonPathToken.Union
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

object JsonPathParser {

  case object default extends JsonPathParser with StandardExtensions
}
