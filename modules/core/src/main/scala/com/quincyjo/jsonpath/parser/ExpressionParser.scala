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

import cats.data.ValidatedNel
import cats.implicits._
import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.extensions.{Extension, FunctionExtension}
import com.quincyjo.jsonpath.extensions.Extension.InvalidArgs
import com.quincyjo.jsonpath.parser.models.ExpressionParseContext.ExpressionToken
import com.quincyjo.jsonpath.parser.models._
import com.quincyjo.jsonpath.{Expression, JsonPath}

import scala.annotation.tailrec
import scala.collection.mutable

final case class ExpressionParser(
    extensions: List[Extension[?, ?]] = List.empty
) {

  def parse(string: String): ParseResult[Expression] = {

    @tailrec
    def go(
        context: ExpressionParseContext,
        stack: mutable.Stack[ValueAt[Expression]],
        pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
    ): ParseResult[ExpressionParseContext] = {
      context.currentTokenOrEndOfInput
        .flatMap {
          case token: ExpressionToken.BinaryToken =>
            pending.push(ValueAt(token, context.index, token.symbol))
            parseExpression(context.nextToken(), stack, pending)
          case _ =>
            parseExpression(context, stack, pending)
        } match {
        case error: ParseError                   => error
        case Parsed(context) if !context.hasNext => Parsed(context)
        case Parsed(context)                     => go(context.nextToken(), stack, pending)
      }
    }

    val stack = mutable.Stack.empty[ValueAt[Expression]]
    val pending = mutable.Stack.empty[ValueAt[ExpressionToken.OperatorToken]]
    val context =
      go(ExpressionParseContext(string).nextToken(), stack, pending)

    context.flatMap { context =>
      stack.removeHeadOption() match {
        case Some(expression) if stack.isEmpty => Parsed(expression.value)
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
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
  ): ParseResult[ExpressionParseContext] = {
    context.currentTokenOrEndOfInput
      .flatMap {
        case _: ParserToken.ValueToken =>
          for {
            value <- parseValue(context)
            context <- resolvePending(context, value, stack, pending)
          } yield context
        case ExpressionToken.FunctionExtension =>
          parseFunctionExtension(context, stack, pending)
        case ExpressionToken.CloseParenthesis =>
          pending
            .removeHeadOption()
            .map { foo =>
              foo.value match {
                case ExpressionToken.OpenParenthesis =>
                  resolvePending(
                    context,
                    stack
                      .removeHeadOption()
                      .getOrElse(ValueAt(LiteralNull, 0, "")),
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
            }
            .getOrElse(
              ParseError(
                "Dangling closing parenthesis",
                context.index,
                context.input
              )
            )
        case token: ExpressionToken.OperatorToken =>
          pending.push(ValueAt(token, context.index, token.symbol))
          parseExpression(context.nextToken(), stack, pending)
        case invalidToken =>
          ParseError(
            s"Unexpected token: $invalidToken",
            context.index,
            context.input
          )
      }
  }

  private def parseFunctionExtension(
      context: ExpressionParseContext,
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
  ): ParseResult[ExpressionParseContext] =
    for {
      functionName <- context.valueAsExtensionFunctionName
      parametersAndContext <- parseFunctionExtensionParameters(
        context.nextToken(),
        stack,
        pending
      )
      (context, parameters) = parametersAndContext
      parser <- parseExtension.lift(functionName.value) match {
        case Some(parser) =>
          Parsed(parser)
        case None =>
          ParseError(
            s"Unknown function: ${functionName.value}",
            functionName.index,
            context.input
          )
      }
      func <- parser(parameters).fold(
        invalidArgs => {
          val message = invalidArgs
            .map {
              case InvalidArgs.InvalidArg(arg, message) =>
                s"invalid argument '${arg.value}': $message"
              case InvalidArgs.MissingArg(message) =>
                message
            }
            .toList
            .mkString(", ")
          ParseError(
            s"function '${functionName.value}' $message",
            functionName.index,
            context.input
          )
        },
        Parsed.apply
      )
      _ = stack
        .push(
          ValueAt(func, context.index, functionName.value)
        )
    } yield context

  private def parseFunctionExtensionParameters(
      context: ExpressionParseContext,
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
  ): ParseResult[(ExpressionParseContext, List[ValueAt[Expression]])] = {

    @tailrec
    def go(
        context: ExpressionParseContext,
        builder: mutable.Builder[ValueAt[Expression], List[ValueAt[Expression]]]
    ): ParseResult[(ExpressionParseContext, List[ValueAt[Expression]])] = {
      parseExpression(context, stack, pending) match {
        case error: ParseError => error
        case Parsed(context) =>
          val next = context.nextToken()
          next.currentTokenOrEndOfInput match {
            case error: ParseError => error
            case Parsed(token) =>
              builder.addOne(stack.pop())
              token match {
                case ExpressionToken.CloseParenthesis =>
                  Parsed(next -> builder.result())
                case ExpressionToken.Comma => go(next.nextToken(), builder)
                case invalid =>
                  ParseError.invalidToken(
                    invalid,
                    next.index,
                    next.input,
                    ExpressionToken.CloseParenthesis,
                    ExpressionToken.Comma
                  )
              }
          }
      }
    }

    context.currentTokenOrEndOfInput.flatMap {
      case ExpressionToken.OpenParenthesis =>
        context.peek.foldF(
          ParseError("Unexpected end of input.", context.index, context.input)
        ) {
          case ExpressionToken.CloseParenthesis =>
            Parsed(context -> List.empty)
          case _ =>
            go(context.nextToken(), List.newBuilder[ValueAt[Expression]])
        }
      case other =>
        ParseError.invalidToken(
          other,
          context.index,
          context.input,
          ExpressionToken.OpenParenthesis
        )
    }
  }

  private val parseExtension: PartialFunction[String, List[
    ValueAt[Expression]
  ] => ValidatedNel[InvalidArgs, FunctionExtension[?] & Expression]] =
    extensions
      .fold(PartialFunction.empty) { case (extension, acc) =>
        acc orElse extension
      }

  @tailrec
  private def resolvePending(
      context: ExpressionParseContext,
      value: ValueAt[Expression],
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
  ): ParseResult[ExpressionParseContext] =
    pending.headOption
      .map { head =>
        if (head.value == ExpressionToken.OpenParenthesis)
          head
        else pending.pop()
      }
      .fold[ParseResult[ValueAt[Expression]]](Parsed(value)) { pending =>
        pending.value match {
          case ExpressionToken.OpenParenthesis =>
            Parsed(value)
          case ExpressionToken.Not =>
            value.value match {
              case logical: LogicalType =>
                Parsed(
                  ValueAt(
                    Not(logical),
                    pending.index,
                    pending.raw.concat(value.raw)
                  )
                )
              case other =>
                ParseError(
                  s"! operator requires a logical value but was '$other'",
                  context.index,
                  context.input
                )
            }
          case token: ExpressionToken.BinaryToken =>
            stack
              .removeHeadOption()
              .map { left =>
                makeBinaryOperator(
                  context,
                  pending.copy(value = token),
                  left,
                  value
                )
                  .map(ValueAt(_, context.index, context.input))
              }
              .getOrElse(
                ParseError(
                  s"Unexpected token: $token",
                  context.index,
                  context.input
                )
              )
        }
      } match {
      case error: ParseError => error
      case Parsed(expression) =>
        if (
          pending.headOption.forall(_.value == ExpressionToken.OpenParenthesis)
        ) {
          expression match {
            case ValueAt(And(Or(l1, l2), right), index, raw) =>
              stack.push(ValueAt(Or(l1, And(l2, right)), index, raw))
            case other =>
              stack.push(other)
          }
          Parsed(context)
        } else {
          resolvePending(context, expression, stack, pending)
        }
    }

  private def makeBinaryOperator(
      context: ExpressionParseContext,
      token: ValueAt[ExpressionToken.BinaryToken],
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[Expression] =
    token.value match {
      case ExpressionToken.Plus | ExpressionToken.Minus |
          ExpressionToken.Multiply | ExpressionToken.Divide =>
        makeValueOperator(context, token, left, right)
      case _ => makeLogicalOperator(context, token, left, right)
    }

  private def makeValueOperator(
      context: ExpressionParseContext,
      token: ValueAt[ExpressionToken.BinaryToken],
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[Expression] =
    Option(token.value)
      .collect[(ValueType, ValueType) => BinaryOperator[
        ValueType,
        ValueType
      ] & ValueType] {
        case ExpressionToken.Plus     => Plus.apply
        case ExpressionToken.Minus    => Minus.apply
        case ExpressionToken.Multiply => Multiply.apply
        case ExpressionToken.Divide   => Divide.apply
      }
      .map { cons =>
        (enforceValueType(context, left), enforceValueType(context, right))
          .mapN(cons)
      }
      .getOrElse(ParseError("Unknown operator", 0, ""))

  private def makeLogicalOperator(
      context: ExpressionParseContext,
      token: ValueAt[ExpressionToken.BinaryToken],
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[
    Expression & LogicalType
  ] = {
    Option(token.value)
      .collect[(ValueType, ValueType) => BinaryOperator[
        Expression,
        Expression
      ] & LogicalType] {
        case ExpressionToken.Equal    => Equal.apply
        case ExpressionToken.NotEqual => NotEqual.apply
        case ExpressionToken.LessThan => LessThan.apply
        case ExpressionToken.LessThanOrEqualTo =>
          LessThanOrEqualTo.apply
        case ExpressionToken.GreaterThan =>
          GreaterThan.apply
        case ExpressionToken.GreaterThanOrEqualTo =>
          GreaterThanOrEqualTo.apply
      }
      .map { cons =>
        (enforceValueType(context, left), enforceValueType(context, right))
          .mapN(cons)
      } orElse
      Option(token.value)
        .collect[(LogicalType, LogicalType) => BinaryOperator[
          Expression,
          Expression
        ] & LogicalType] {
          case ExpressionToken.And => And.apply
          case ExpressionToken.Or  => Or.apply
        }
        .map { cons =>
          (
            enforceLogicalType(context, left),
            enforceLogicalType(context, right)
          ).mapN(cons)
        } getOrElse ParseError(
        "Unknown operator",
        token.index,
        context.input
      )
  }

  private def parseValue(
      context: ExpressionParseContext
  ): ParseResult[ValueAt[Expression]] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case ExpressionToken.ValueString =>
          context.valueAsString.map(_.map(string => LiteralString(string)))
        case ExpressionToken.ValueBoolean =>
          context.valueAsBoolean.map(
            _.map(boolean => LiteralBoolean(boolean))
          )
        case ExpressionToken.ValueNumber =>
          context.valueAsNumber.map(
            _.map(number => LiteralNumber(number))
          )
        case ExpressionToken.Root | ExpressionToken.Current =>
          context.valueAsJsonPath.map(_.map {
            case singularQuery: JsonPath.SingularQuery =>
              JsonPathValue(singularQuery)
            case query: JsonPath.Query => JsonPathNodes(query)
          })
        case otherToken =>
          ParseError(
            s"Unexpected token: $otherToken",
            context.index,
            context.input
          )
      }

  private def enforceValueType(
      context: ExpressionParseContext,
      expression: ValueAt[Expression]
  ): ParseResult[ValueType] =
    expression.value match {
      case value: ValueType   => Parsed(value)
      case foo: JsonPathValue => Parsed(foo)
      case other =>
        ParseError(
          s"Required value type but found $other",
          expression.index,
          context.input
        )
    }

  private def enforceLogicalType(
      context: ExpressionParseContext,
      expression: ValueAt[Expression]
  ): ParseResult[LogicalType] =
    expression.value match {
      case value: LogicalType => Parsed(value)
      case foo: JsonPathValue => Parsed(foo)
      case foo: JsonPathNodes => Parsed(foo)
      case other =>
        ParseError(
          s"Required logical type but found $other",
          expression.index,
          context.input
        )
    }

}

object ExpressionParser {

  final val default = new ExpressionParser()
}
