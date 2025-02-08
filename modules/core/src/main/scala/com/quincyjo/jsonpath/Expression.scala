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

package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.JsonSupport.Implicits.JsonSupportOps
import com.quincyjo.jsonpath.parser.util.StringEscapes

sealed trait Expression {

  /*
  def apply[Json: JsonSupport](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Json
   */
}

object Expression {

  sealed trait EvaluatesTo[T] {

    def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): T
  }

  sealed trait ExpressionType

  // JSON atomic literal or Nothing
  // Can be coerced from a singular query.
  trait ValueType extends ExpressionType with EvaluatesTo[Option[?]] {

    def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json]
  }

  object ValueType {

    // TODO: Temp workaround for value comparison
    private final case class ValueTypeFromNodesType(
        jsonPathValue: JsonPathValue
    ) extends ValueType {

      override def apply[Json: JsonSupport](
          evaluator: JsonPathEvaluator[Json],
          root: Json,
          current: Json
      ): Option[Json] =
        jsonPathValue(evaluator, root, current).headOption

      override def toString: String =
        jsonPathValue.toString
    }

    implicit def jsonPathValueToValueType(
        jsonPathValue: JsonPathValue
    ): ValueType = ValueTypeFromNodesType(jsonPathValue)
  }

  // Logical true or false, distinct from JSON true or false
  // Can be coerced from a NodesType via size of at least 1
  trait LogicalType extends ExpressionType with EvaluatesTo[Boolean]

  object LogicalType {

    // TODO: Temp workaround for value comparison
    private final case class LogicalTypeFromNodesType(nodesType: NodesType)
        extends LogicalType {
      override def apply[Json: JsonSupport](
          evaluator: JsonPathEvaluator[Json],
          root: Json,
          current: Json
      ): Boolean =
        nodesType(evaluator, root, current).nonEmpty

      override def toString: String =
        nodesType.toString
    }

    implicit def nodesTypeToLogicalType(nodesType: NodesType): LogicalType =
      LogicalTypeFromNodesType(nodesType)
  }

  // JSONPath results as inputs, eg value(@['foobar'])
  trait NodesType extends ExpressionType with EvaluatesTo[List[?]] {

    def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Json]
  }

  sealed trait UnaryOperator[ParamType <: ExpressionType] extends Expression {

    def symbol: String

    def expression: ParamType

    override def toString: String =
      expression match {
        case binary: BinaryOperator[_, _] => s"$symbol($binary)"
        case expression                   => s"$symbol$expression"
      }
  }

  sealed trait BinaryOperator[
      +LeftType <: ExpressionType,
      +RightType <: ExpressionType
  ] extends Expression {

    def symbol: String

    def left: LeftType

    def right: RightType

    override def toString: String =
      s"$left $symbol ${right match {
        case other: BinaryOperator[_, _] => s"(${other.toString})"
        case value                       => value.toString
      }}"
  }

  // Literals, singular queries, functions of ValueType
  trait Comparable

  // Extends LogicalType
  sealed trait Comparator
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType {

    protected def compare[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (Int, Int) => Boolean): Boolean = {
      val leftResult = left(evaluator, root, current)
      val rightResult = right(evaluator, root, current)
      leftResult
        .flatMap(_.asString)
        .zip(rightResult.flatMap(_.asString))
        .map { case (l, r) => l compareTo r }
        .orElse(
          leftResult
            .flatMap(_.asArray)
            .zip(rightResult.flatMap(_.asArray))
            // TODO: Deep value comparison
            .map { case (l, r) => l.size compareTo r.size }
        )
        .orElse(
          leftResult
            .flatMap(_.coerceToNumber)
            .zip(rightResult.flatMap(_.coerceToNumber))
            .map { case (l, r) => l compareTo r }
        )
        .fold(this match {
          case _: IncludesEqualityCheck =>
            leftResult.isEmpty && rightResult.isEmpty
          case _ => false
        })(f(_, 0))
    }
  }

  sealed trait IncludesEqualityCheck extends Comparator

  // Open trait for extension functions.
  trait Extension

  sealed trait Literal extends Expression with ValueType {

    def asJson[Json: JsonSupport]: Json

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] = Some(asJson[Json])
  }

  case object LiteralNull extends Literal {

    override def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].Null

    override def toString: String = "null"
  }

  final case class LiteralString(value: String) extends Literal {

    override def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].string(value)

    override def toString: String =
      s"""\"${StringEscapes.escapeDoubleQuotes(value)}\""""
  }

  final case class LiteralNumber(value: BigDecimal) extends Literal {

    def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].number(value)

    override def toString: String = value.toString
  }

  final case class LiteralBoolean(value: Boolean) extends Literal {

    def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].boolean(value)

    override def toString: String = value.toString
  }

  final case class JsonPathValue(path: JsonPath.SingularQuery)
      extends Expression
      with NodesType {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Json] =
      evaluator
        .evaluate(path, root, Some(current))

    override def toString: String =
      path.toString
  }

  final case class JsonPathNodes(path: JsonPath.Query)
      extends Expression
      with NodesType {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Json] =
      evaluator
        .evaluate(path, root, Some(current))

    override def toString: String =
      path.toString
  }

  final case class Not(expression: LogicalType)
      extends UnaryOperator[LogicalType]
      with LogicalType {

    override def symbol: String = "!"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      !expression(evaluator, root, current)
  }

  final case class Equal(left: ValueType, right: ValueType)
      extends Comparator
      with IncludesEqualityCheck {

    override def symbol: String = "=="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) ->
        right(evaluator, root, current) match {
        case None -> None       => true
        case Some(l) -> Some(r) => l == r
        case _                  => false
      }
  }

  final case class NotEqual(left: ValueType, right: ValueType)
      extends Comparator {

    override def symbol: String = "!="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) ->
        right(evaluator, root, current) match {
        case None -> None       => false
        case Some(l) -> Some(r) => l != r
        case _                  => true
      }
  }

  final case class GreaterThan(left: ValueType, right: ValueType)
      extends Comparator {

    override val symbol: String = ">"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ > _)
  }

  final case class GreaterThanOrEqualTo(left: ValueType, right: ValueType)
      extends Comparator
      with IncludesEqualityCheck {

    override val symbol: String = ">="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ >= _)
  }

  final case class LessThan(left: ValueType, right: ValueType)
      extends Comparator {

    override val symbol: String = "<"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ < _)
  }

  final case class LessThanOrEqualTo(left: ValueType, right: ValueType)
      extends Comparator
      with IncludesEqualityCheck {

    override val symbol: String = "<="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ <= _)
  }

  final case class And(left: LogicalType, right: LogicalType)
      extends BinaryOperator[LogicalType, LogicalType]
      with LogicalType {

    override def symbol: String = "&&"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) &&
        right(evaluator, root, current)
  }

  final case class Or(left: LogicalType, right: LogicalType)
      extends BinaryOperator[LogicalType, LogicalType]
      with LogicalType {

    override def symbol: String = "||"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) ||
        right(evaluator, root, current)
  }

  sealed trait ArithmeticOperator
      extends BinaryOperator[ValueType, ValueType]
      with ValueType {

    protected def arithmetic[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (BigDecimal, BigDecimal) => BigDecimal): Option[Json] =
      left(evaluator, root, current)
        .flatMap(_.coerceToNumber)
        .zip(right(evaluator, root, current).flatMap(_.coerceToNumber))
        .map { case (left, right) =>
          implicitly[JsonSupport[Json]].number(f(left, right))
        }
  }

  final case class Plus(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with ValueType {

    override def symbol: String = "+"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] = {
      left(evaluator, root, current).zip(right(evaluator, root, current)).map {
        case (leftResult, rightResult) =>
          if (
            (leftResult.isNumber || leftResult.isNull) &&
            (rightResult.isNumber || rightResult.isNull)
          )
            implicitly[JsonSupport[Json]].number(
              leftResult.coerceToNumber.getOrElse(BigDecimal(0)) +
                rightResult.coerceToNumber.getOrElse(BigDecimal(0))
            )
          else
            implicitly[JsonSupport[Json]].string(
              leftResult.coerceToString concat rightResult.coerceToString
            )
      }
    }
  }

  final case class Minus(left: ValueType, right: ValueType)
      extends ArithmeticOperator {

    override def symbol: String = "-"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ - _)
  }

  final case class Divide(left: ValueType, right: ValueType)
      extends ArithmeticOperator {

    override def symbol: String = "/"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ / _)
  }

  final case class Multiply(left: ValueType, right: ValueType)
      extends ArithmeticOperator {

    override def symbol: String = "*"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ * _)
  }
}
