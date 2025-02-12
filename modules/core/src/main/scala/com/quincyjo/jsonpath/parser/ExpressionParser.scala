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
import com.quincyjo.jsonpath.extensions.Extension.InvalidArgs
import com.quincyjo.jsonpath.extensions.{Extension, FunctionExtension}
import com.quincyjo.jsonpath.parser.models.ExpressionParseContext.ExpressionToken
import com.quincyjo.jsonpath.parser.models._
import com.quincyjo.jsonpath.{Expression, JsonPath}

import scala.annotation.tailrec
import scala.collection.mutable

final case class ExpressionParser(
    extensions: List[Extension[?, ?]] = List.empty,
    jsonPathParser: JsonPathParser
) {

  def parse(string: String): ParseResult[Expression] = {

    val stack = mutable.Stack.empty[ValueAt[Expression]]
    val pending = mutable.Stack.empty[ValueAt[ExpressionToken.OperatorToken]]

    parseExpressionComplete(
      ExpressionParseContext(string, jsonPathParser).nextToken(),
      stack,
      pending
    )
      .flatMap { context =>
        stack.removeHeadOption() match {
          case Some(expression) if stack.isEmpty => Parsed(expression.value)
          case _ =>
            pending.headOption.fold(
              ParseError("Unexpected end of input.", context.index, string)
            ) { pending =>
              ParseError(
                s"Unresolved operator '$pending'",
                context.index,
                string
              )
            }
        }
      }
  }

  /** Parses expressions continuously until a token is encountered that matches
    * the `endOn` predicate or a failure is encountered. If the end token was
    * encountered, the context is pointing at it. Otherwise, the context will be
    * pointing at the next token.
    * @param context
    *   The current parse context.
    * @param stack
    *   The stack of parsed expressions.
    * @param pending
    *   The pending operators.
    * @param endOn
    *   The predicate to determine when to stop parsing.
    * @return
    *   The final context. If the end token was encountered, the context is
    *   pointing at it. Otherwise, the context will be pointing at the next
    *   token.
    */
  @tailrec
  private def parseExpressionComplete(
      context: ExpressionParseContext,
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]],
      endOn: ExpressionToken => Boolean = _ => false
  ): ParseResult[ExpressionParseContext] = {
    context.currentTokenOrEndOfInput
      .flatMap {
        case token if endOn(token) =>
          Parsed(context)
        case token: ExpressionToken.BinaryToken =>
          pending.push(ValueAt(token, context.index, token.symbol))
          parseExpression(context.nextToken(), stack, pending)
        case _ =>
          parseExpression(context, stack, pending)
      } match {
      case error: ParseError => error
      case Parsed(context)
          if context.currentTokenResult.isEmpty.getOrElse(true) ||
            context.currentTokenResult.exists(endOn).getOrElse(false) =>
        Parsed(context)
      case Parsed(context) =>
        parseExpressionComplete(context, stack, pending, endOn)
    }
  }

  // TODO: tailrec
  private def parseExpression(
      context: ExpressionParseContext,
      stack: mutable.Stack[ValueAt[Expression]],
      pending: mutable.Stack[ValueAt[ExpressionToken.OperatorToken]]
  ): ParseResult[ExpressionParseContext] =
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
            .map { pendingOperator =>
              pendingOperator.value match {
                case ExpressionToken.OpenParenthesis =>
                  stack.removeHeadOption() match {
                    case Some(expression) =>
                      resolvePending(context, expression, stack, pending)
                    case None =>
                      ParseError(
                        s"Empty parentheses. Parentheses must contain some expression.",
                        context.index,
                        context.input
                      )
                  }
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
      _ = stack.push(
        ValueAt(
          func,
          functionName.index,
          context.input.substring(functionName.index, context.index + 1)
        )
      )
    } yield context.nextToken()

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
      parseExpressionComplete(
        context,
        stack,
        pending,
        token => {
          token == ExpressionToken.CloseParenthesis || token == ExpressionToken.Comma
        }
      ) match {
        case error: ParseError => error
        case Parsed(context) =>
          context.currentTokenOrEndOfInput match {
            case error: ParseError => error
            case Parsed(token) =>
              builder.addOne(stack.pop())
              token match {
                case ExpressionToken.CloseParenthesis =>
                  Parsed(context -> builder.result())
                case ExpressionToken.Comma =>
                  go(context.nextToken(), builder)
                case invalid =>
                  ParseError.invalidToken(
                    invalid,
                    context.index,
                    context.input,
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
            enforceLogicalType(context, value).map { logicalType =>
              ValueAt(
                Not(logicalType),
                pending.index,
                context.input.substring(
                  pending.index,
                  value.index + value.raw.length
                )
              )
            }
          case token: ExpressionToken.BinaryToken =>
            stack
              .removeHeadOption()
              .map { left =>
                parseBinaryOperator(
                  context,
                  pending.copy(value = token),
                  left,
                  value
                )
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
          Parsed(context.nextToken())
        } else {
          resolvePending(context, expression, stack, pending)
        }
    }

  private def parseBinaryOperator(
      context: ExpressionParseContext,
      token: ValueAt[ExpressionToken.BinaryToken],
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[ValueAt[Expression]] =
    (token.value match {
      case arithmetic: ExpressionToken.ArithmeticToken =>
        parseArithmeticExpression(context, arithmetic, left, right)
      case comparator: ExpressionToken.ComparatorToken =>
        parseComparatorOperator(context, comparator, left, right)
      case logical: ExpressionToken.LogicalToken =>
        parseLogicalOperator(context, logical, left, right)
    }).map { expression =>
      ValueAt(
        expression,
        left.index,
        context.input.substring(
          left.index,
          right.index + right.raw.length
        )
      )
    }

  private def parseArithmeticExpression(
      context: ExpressionParseContext,
      token: ExpressionToken.ArithmeticToken,
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[Expression & ValueType] =
    (enforceValueType(context, left), enforceValueType(context, right))
      .mapN { case (left, right) =>
        token match {
          case ExpressionToken.Plus     => Plus(left, right)
          case ExpressionToken.Minus    => Minus(left, right)
          case ExpressionToken.Multiply => Multiply(left, right)
          case ExpressionToken.Divide   => Divide(left, right)
        }
      }

  private def parseComparatorOperator(
      context: ExpressionParseContext,
      token: ExpressionToken.ComparatorToken,
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[Expression & LogicalType] =
    (enforceValueType(context, left), enforceValueType(context, right))
      .mapN { case (left, right) =>
        token match {
          case ExpressionToken.Equal =>
            Equal(left, right)
          case ExpressionToken.NotEqual =>
            NotEqual(left, right)
          case ExpressionToken.LessThan =>
            LessThan(left, right)
          case ExpressionToken.LessThanOrEqualTo =>
            LessThanOrEqualTo(left, right)
          case ExpressionToken.GreaterThan =>
            GreaterThan(left, right)
          case ExpressionToken.GreaterThanOrEqualTo =>
            GreaterThanOrEqualTo(left, right)
        }
      }

  private def parseLogicalOperator(
      context: ExpressionParseContext,
      token: ExpressionToken.LogicalToken,
      left: ValueAt[Expression],
      right: ValueAt[Expression]
  ): ParseResult[Expression & LogicalType] =
    (enforceLogicalType(context, left), enforceLogicalType(context, right))
      .mapN { case (left, right) =>
        token match {
          case ExpressionToken.And => And(left, right)
          case ExpressionToken.Or  => Or(left, right)
        }
      }

  private def parseValue(
      context: ExpressionParseContext
  ): ParseResult[ValueAt[Expression]] =
    context.currentTokenOrEndOfInput
      .flatMap {
        case ExpressionToken.ValueString =>
          context.valueAsString.map(
            _.map(LiteralString.apply)
          )
        case ExpressionToken.ValueBoolean =>
          context.valueAsBoolean.map(
            _.map(LiteralBoolean.apply)
          )
        case ExpressionToken.ValueNumber =>
          context.valueAsNumber.map(
            _.map(LiteralNumber.apply)
          )
        case ExpressionToken.Root | ExpressionToken.Current =>
          context.valueAsJsonPath.map(_.map {
            case singularQuery: JsonPath.SingularQuery =>
              JsonPathValue(singularQuery)
            case query: JsonPath.Query =>
              JsonPathNodes(query)
          })
        case otherToken =>
          ParseError.invalidToken(
            otherToken,
            context.index,
            context.input,
            ExpressionToken.ValueString,
            ExpressionToken.ValueBoolean,
            ExpressionToken.ValueNumber,
            ExpressionToken.Root,
            ExpressionToken.Current
          )
      }

  private def enforceValueType(
      context: ExpressionParseContext,
      expression: ValueAt[Expression]
  ): ParseResult[ValueType] =
    ValueType
      .coerce(expression.value)
      .map(Parsed.apply)
      .getOrElse(
        ParseError(
          s"Required value type but found ${expression.value}",
          expression.index,
          context.input
        )
      )

  private def enforceLogicalType(
      context: ExpressionParseContext,
      expression: ValueAt[Expression]
  ): ParseResult[LogicalType] =
    LogicalType
      .coerce(expression.value)
      .map(Parsed.apply)
      .getOrElse(
        ParseError(
          s"Required logical type but found ${expression.value}",
          expression.index,
          context.input
        )
      )
}
