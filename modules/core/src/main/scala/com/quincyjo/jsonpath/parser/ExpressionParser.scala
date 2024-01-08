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
import com.quincyjo.jsonpath.Expression
import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.parser.models.ExpressionParseContext.ExpressionToken
import com.quincyjo.jsonpath.parser.models._

import scala.annotation.tailrec
import scala.collection.mutable

object ExpressionParser {

  def parse(string: String): ParseResult[Expression] = {

    @tailrec
    def go(
        context: ExpressionParseContext,
        stack: mutable.Stack[Expression],
        pending: mutable.Stack[ExpressionToken.OperatorToken]
    ): ParseResult[ExpressionParseContext] = {
      context.currentTokenOrEndOfInput
        .flatMap {
          case token: ExpressionToken.BinaryToken =>
            pending.push(token)
            parseExpression(context.nextToken(), stack, pending)
          case _ =>
            parseExpression(context, stack, pending)
        } match {
        case error: ParseError                   => error
        case Parsed(context) if !context.hasNext => Parsed(context)
        case Parsed(context)                     => go(context.nextToken(), stack, pending)
      }
    }

    val stack = mutable.Stack.empty[Expression]
    val pending = mutable.Stack.empty[ExpressionToken.OperatorToken]
    val context =
      go(ExpressionParseContext(string).nextToken(), stack, pending)

    context.flatMap { context =>
      stack.removeHeadOption() match {
        case Some(expression) if stack.isEmpty => Parsed(expression)
        case None =>
          ParseError("Unexpected end of input.", context.index, string)
        case _ =>
          ParseError(
            s"Unresolved operator '${pending.headOption}'",
            context.index,
            string
          )
      }
    }
  }

  private def parseExpression(
      context: ExpressionParseContext,
      stack: mutable.Stack[Expression],
      pending: mutable.Stack[ExpressionToken.OperatorToken]
  ): ParseResult[ExpressionParseContext] = {
    context.currentTokenOrEndOfInput
      .flatMap {
        case invalidToken: ExpressionToken.BinaryToken =>
          ParseError(
            s"Unexpected token: $invalidToken",
            context.index,
            context.input
          )
        case _: ParserToken.ValueToken =>
          for {
            value <- parseValue(context)
            context <- resolvePending(value, context, stack, pending)
          } yield context
        case ExpressionToken.CloseParenthesis =>
          pending
            .removeHeadOption()
            .map {
              case ExpressionToken.OpenParenthesis =>
                resolvePending(
                  stack.removeHeadOption().getOrElse(JsonNull),
                  context,
                  stack,
                  pending
                )
                Parsed(context)
              case trailingToken =>
                ParseError(
                  s"Trailing token: $trailingToken",
                  context.index,
                  context.input
                )
            }
            .getOrElse(
              ParseError(
                "Dangling closing parenthesis",
                context.index,
                context.input
              )
            )
        case token: ExpressionToken.OperatorToken =>
          pending.push(token)
          parseExpression(context.nextToken(), stack, pending)
      }
  }

  @tailrec
  private def resolvePending(
      value: Expression,
      context: ExpressionParseContext,
      stack: mutable.Stack[Expression],
      pending: mutable.Stack[ExpressionToken.OperatorToken]
  ): ParseResult[ExpressionParseContext] =
    pending.headOption
      .map {
        case ExpressionToken.OpenParenthesis =>
          ExpressionToken.OpenParenthesis
        case _ => pending.pop()
      }
      .fold[ParseResult[Expression]](Parsed(value)) {
        case ExpressionToken.OpenParenthesis =>
          Parsed(value)
        case ExpressionToken.Not =>
          Parsed(Not(value))
        case token: ExpressionToken.BinaryToken =>
          stack
            .removeHeadOption()
            .map { left =>
              Parsed(makeBinaryOperator(token, left, value))
            }
            .getOrElse(
              ParseError(
                s"Unexpected token: $token",
                context.index,
                context.input
              )
            )
      } match {
      case error: ParseError => error
      case Parsed(expression) =>
        if (pending.headOption.forall(_ == ExpressionToken.OpenParenthesis)) {
          stack.push(expression)
          Parsed(context)
        } else {
          resolvePending(expression, context, stack, pending)
        }
    }

  private def makeBinaryOperator(
      token: ExpressionToken.BinaryToken,
      left: Expression,
      right: Expression
  ): BinaryOperator = token match {
    case ExpressionToken.And      => And(left, right)
    case ExpressionToken.Or       => Or(left, right)
    case ExpressionToken.Equal    => Equal(left, right)
    case ExpressionToken.NotEqual => NotEqual(left, right)
    case ExpressionToken.LessThan => LessThan(left, right)
    case ExpressionToken.LessThanOrEqualTo =>
      LessThanOrEqualTo(left, right)
    case ExpressionToken.GreaterThan =>
      GreaterThan(left, right)
    case ExpressionToken.GreaterThanOrEqualTo =>
      GreaterThanOrEqualTo(left, right)
    case ExpressionToken.Plus     => Plus(left, right)
    case ExpressionToken.Minus    => Minus(left, right)
    case ExpressionToken.Multiply => Multiply(left, right)
    case ExpressionToken.Divide   => Divide(left, right)
  }

  private def parseValue(context: ExpressionParseContext): ParseResult[Value] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case ExpressionToken.ValueString =>
          context.valueAsString.map(string => JsonString(string.value))
        case ExpressionToken.ValueBoolean =>
          context.valueAsBoolean.map(boolean => JsonBoolean(boolean.value))
        case ExpressionToken.ValueNumber =>
          context.valueAsNumber.map(number => JsonNumber(number.value))
        case ExpressionToken.Root | ExpressionToken.Current =>
          context.valueAsJsonPath.map(jsonPath => JsonPathValue(jsonPath.value))
        case otherToken =>
          ParseError(
            s"Unexpected token: $otherToken",
            context.index,
            context.input
          )
      }
}
