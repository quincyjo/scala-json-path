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

package com.quincyjo.jsonpath.parser.models

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.Expression
import com.quincyjo.jsonpath.parser.ExpressionParser
import com.quincyjo.jsonpath.parser.models.JsonPathParseContext._
import com.quincyjo.jsonpath.parser.util.BalancedExpressionReader

private[parser] final case class JsonPathParseContext private (
    input: String,
    index: Int,
    currentTokenResult: OptionT[ParseResult, JsonPathToken],
    expressionParser: ExpressionParser
) extends ParseContext[JsonPathToken] {

  override def nextToken(): JsonPathParseContext = {
    val newIndex = nextIndex
    copy(
      index = newIndex.getOrElse(index),
      currentTokenResult = OptionT.liftF(newIndex.flatMap(tokenAt))
    )
  }

  def valueAsString: ParseResult[ValueAt[String]] =
    valueAs { case JsonPathToken.ValueString =>
      input.charAt(index) match {
        case _ @('\'' | '"') =>
          parseQuotedString(index)
        case _ =>
          val source = input
            .substring(index)
            .takeWhile(c => c.isLetterOrDigit || c == '_')
          Parsed(ValueAt(source, index, source))
      }
    }

  def valueAsNumber: ParseResult[ValueAt[Int]] =
    valueAs { case JsonPathToken.ValueInt =>
      val source = input.charAt(index) match {
        case '-' =>
          input
            .substring(index + 1)
            .takeWhile(_.isDigit)
            .prepended('-')
        case _ => input.substring(index).takeWhile(_.isDigit)
      }
      source.toIntOption
        .map { int =>
          Parsed(ValueAt(int, index, source))
        }
        .getOrElse(
          ParseError(
            s"Invalid number '$source' starting at right $index.",
            index,
            input
          )
        )
    }

  def valueAsExpression: ParseResult[ValueAt[Expression]] =
    valueAs {
      case JsonPathToken.StartExpression =>
        val balanced =
          BalancedExpressionReader(input.substring(index)).takeGroup
        expressionParser.parse(balanced).map { expression =>
          ValueAt(expression, index, balanced)
        }
      case JsonPathToken.StartFilterExpression =>
        val hasParens = input.lift(index + 1).contains('(')
        val balanced =
          if (hasParens)
            BalancedExpressionReader(input.substring(index + 1)).takeGroup
          else
            BalancedExpressionReader(input.substring(index + 1))
              .takeUntil(char => char == ']' || char == ',')
        expressionParser.parse(balanced) match {
          case Parsed(expression) =>
            Parsed(ValueAt(expression, index + 1, s"?$balanced"))
          case ParseError(message, index, input, cause) =>
            ParseError(message, index + 1, input, cause)
        }
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          index,
          input,
          JsonPathToken.StartExpression,
          JsonPathToken.StartFilterExpression
        )
    }

  override def tokenAt(i: Int): ParseResult[JsonPathToken] =
    input.charAt(i) match {
      case '$' => Parsed(JsonPathToken.Root)
      case '@' => Parsed(JsonPathToken.Current)
      case '*' => Parsed(JsonPathToken.Wildcard)
      case '[' => Parsed(JsonPathToken.StartSelector)
      case ']' => Parsed(JsonPathToken.EndSelector)
      case '.' =>
        if (input.lift(i + 1).contains('.'))
          Parsed(JsonPathToken.RecursiveDescent)
        else Parsed(JsonPathToken.DotSelector)
      case '?' => Parsed(JsonPathToken.StartFilterExpression)
      case '(' => Parsed(JsonPathToken.StartExpression)
      // case ')'                                    => Parsed(JsonPathToken.EndExpression)
      case ','             => Parsed(JsonPathToken.Union)
      case ':'             => Parsed(JsonPathToken.Slice)
      case '\'' | '"'      => Parsed(JsonPathToken.ValueString)
      case c if c.isLetter => Parsed(JsonPathToken.ValueString)
      case '-'             => Parsed(JsonPathToken.ValueInt)
      case c if c.isDigit  => Parsed(JsonPathToken.ValueInt)
      case c =>
        ParseError(
          s"Invalid character '$c' at right $i in '$input'.",
          i,
          input
        )
    }

  override def value(): ParseResult[ValueAt[Any]] =
    valueAs {
      case JsonPathToken.ValueInt =>
        valueAsNumber
      case JsonPathToken.ValueString =>
        valueAsString
      case JsonPathToken.StartExpression |
          JsonPathToken.StartFilterExpression =>
        valueAsExpression
    }
}

object JsonPathParseContext {

  def apply(
      input: String,
      expressionParser: ExpressionParser
  ): JsonPathParseContext =
    new JsonPathParseContext(
      input,
      0,
      OptionT.none[ParseResult, JsonPathToken],
      expressionParser
    )

  sealed abstract class JsonPathToken extends ParserToken

  sealed abstract class SymbolToken(override val symbol: String)
      extends JsonPathToken
      with ParserToken.SymbolToken

  sealed trait ValueToken extends JsonPathToken with ParserToken.ValueToken

  object JsonPathToken {

    case object Root extends SymbolToken("$")
    case object Current extends SymbolToken("@")
    case object Wildcard extends SymbolToken("*")
    case object StartSelector extends SymbolToken("[")
    case object EndSelector extends SymbolToken("]")
    case object DotSelector extends SymbolToken(".")
    case object RecursiveDescent extends SymbolToken("..")
    case object Union extends SymbolToken(",")
    case object Slice extends SymbolToken(":")
    case object StartFilterExpression extends ValueToken // SymbolToken("?(")
    case object StartExpression extends ValueToken // SymbolToken("(")
    // final case object EndExpression extends SymbolToken(")")

    case object ValueString extends ValueToken
    case object ValueInt extends ValueToken
  }

}
