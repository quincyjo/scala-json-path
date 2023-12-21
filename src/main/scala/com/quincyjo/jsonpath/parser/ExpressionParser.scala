package com.quincyjo.jsonpath.parser

import cats.{Applicative, Eval, Monad, Traverse}
import cats.data.OptionT
import cats.implicits.*
import com.quincyjo.jsonpath.parser.JsonPathParser.*
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath.*
import scala.collection.mutable

trait ExpressionParser[+E <: JsonPath.Expression] {

  def getValueAsExpression(input: String, index: Int): ParseResult[ValueAt[E]]
}

object ExpressionParser {

  object BalancedExpressionParser extends ExpressionParser[LiteralExpression] {

    override def getValueAsExpression(
      input: String,
      index: Int
    ): ParseResult[ValueAt[LiteralExpression]] =
      BalancedExpressionReader(input.substring(index)).takeGroup match {
        case raw @ s"($expression)" =>
          Parsed(ValueAt(LiteralExpression(expression), index, raw))
        case raw =>
          ParseError(
            s"Expressions must be contained in parentheses, but was '$raw'",
            index,
            input
          )
      }
  }

  object JsonPathExpressionParser extends ExpressionParser[JsonPathExpression] {

    override def getValueAsExpression(
      input: String,
      index: Int
    ): ParseResult[ValueAt[JsonPathExpression]] = {
      BalancedExpressionReader(input.substring(index)).takeGroup match {
        case raw @ s"($expression)" =>
          JsonPathReader(expression, JsonPathParserOptions(expressionParser = this)).parseInput().map { jsonPath =>
            ValueAt(JsonPathExpression(jsonPath), index, raw)
          }
        case raw =>
          ParseError(
            s"Expressions must be contained in parentheses, but was '$raw'",
            index,
            input
          )
      }
    }
  }
}