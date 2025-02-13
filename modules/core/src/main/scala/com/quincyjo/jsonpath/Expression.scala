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

import cats.data.Validated
import com.quincyjo.braid.Braid
import com.quincyjo.braid.implicits._
import com.quincyjo.jsonpath.JsonPath.SingularQuery
import com.quincyjo.jsonpath.parser.util.StringEscapes

sealed trait Expression {

  def as[T <: Expression: Expression.Coercible]: Validated[String, T] =
    Expression.coerceTo[T](this)
}

object Expression {

  final val Null = LiteralNull
  final val True = LiteralBoolean(true)
  final val False = LiteralBoolean(false)

  final case class Coercible[Type](
      coerce: Expression => Validated[String, Type]
  ) extends (Expression => Validated[String, Type]) {

    def apply(expression: Expression): Validated[String, Type] =
      coerce(expression)
  }

  def coerceTo[Type <: Expression: Coercible](
      expression: Expression
  ): Validated[String, Type] =
    implicitly[Coercible[Type]].coerce(expression)

  trait ValueType extends Expression {

    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json]

    def isEqualTo(that: ValueType): Equal =
      Equal(this, that)

    def ===(that: ValueType): Equal =
      isEqualTo(that)

    def isNotEqualTo(that: ValueType): NotEqual =
      NotEqual(this, that)

    def =!=(that: ValueType): NotEqual =
      isNotEqualTo(that)

    def isGreaterThan(that: ValueType): GreaterThan =
      GreaterThan(this, that)

    def >(that: ValueType): GreaterThan =
      isGreaterThan(that)

    def isGreaterThanOrEqualTo(that: ValueType): GreaterThanOrEqualTo =
      GreaterThanOrEqualTo(this, that)

    def >=(that: ValueType): GreaterThanOrEqualTo =
      isGreaterThanOrEqualTo(that)

    def isLessThan(that: ValueType): LessThan =
      LessThan(this, that)

    def <(that: ValueType): LessThan =
      isLessThan(that)

    def isLessThanOrEqualTo(that: ValueType): LessThanOrEqualTo =
      LessThanOrEqualTo(this, that)

    def <=(that: ValueType): LessThanOrEqualTo =
      isLessThanOrEqualTo(that)
  }

  object ValueType {

    implicit val coerceToValueType: Expression.Coercible[ValueType] =
      Expression.Coercible[ValueType](coerce)

    def coerce(expression: Expression): Validated[String, ValueType] =
      expression match {
        case valueType: ValueType => Validated.Valid(valueType)
        case jsonPathValue: JsonPathValue =>
          Validated.Valid(ValueTypeFromNodesType(jsonPathValue))
        case JsonPathNodes(query: SingularQuery) =>
          Validated.Valid(ValueTypeFromNodesType(JsonPathValue(query)))
        case _: NodesType =>
          Validated.invalid(
            "NodesType can only be coerced to ValueType when from a singular query."
          )
        case _: LogicalType =>
          Validated.invalid("LogicalType cannot be coerced to ValueType.")
      }

    // TODO: Are wrapping case classes the best way to represent this?
    private final case class ValueTypeFromNodesType(
        jsonPathValue: JsonPathValue
    ) extends ValueType {

      override def apply[Json: Braid](
          evaluator: JsonPathEvaluator[Json],
          root: Json,
          current: Json
      ): Option[Json] =
        jsonPathValue(evaluator, root, current).headOption.map(_.value)

      override def toString: String =
        jsonPathValue.toString
    }

    implicit def jsonPathValueToValueType(
        jsonPathValue: JsonPathValue
    ): ValueType = ValueTypeFromNodesType(jsonPathValue)
  }

  trait LogicalType extends Expression {

    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean

    def &&(that: LogicalType): And =
      And(this, that)

    def ||(that: LogicalType): Or =
      Or(this, that)

    def unary_! : Not =
      Not(this)
  }

  object LogicalType {

    implicit val coerceToValueType: Expression.Coercible[LogicalType] =
      Expression.Coercible[LogicalType](coerce)

    def coerce(expression: Expression): Validated[String, LogicalType] =
      expression match {
        case logicalType: LogicalType =>
          Validated.Valid(logicalType)
        case nodesType: NodesType =>
          Validated.valid(LogicalTypeFromNodesType(nodesType))
        case _: ValueType =>
          Validated.invalid("ValueType cannot be coerced to LogicalType.")
      }

    // TODO: Are wrapping case classes the best way to represent this?
    private final case class LogicalTypeFromNodesType(nodesType: NodesType)
        extends LogicalType {
      override def apply[Json: Braid](
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

  trait NodesType extends Expression {

    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Node[Json]]

    /** Convenience method for creating a LogicalType from a NodesType.
      * @return
      *   A logical type as existence check.
      */
    def exists: LogicalType =
      LogicalType.nodesTypeToLogicalType(this)
  }

  object NodesType {

    implicit val coerceToValueType: Expression.Coercible[NodesType] =
      Expression.Coercible[NodesType](coerce)

    def coerce(expression: Expression): Validated[String, NodesType] =
      expression match {
        case nodesType: NodesType =>
          Validated.valid(nodesType)
        case _: LogicalType =>
          Validated.invalid("LogicalType cannot be coerced to LogicalType.")
        case _: ValueType =>
          Validated.invalid("ValueType cannot be coerced to LogicalType.")
      }
  }

  private[jsonpath] sealed trait UnaryOperator[ParamType <: Expression] {
    self: Expression =>

    def symbol: String

    def expression: ParamType

    override def toString: String =
      expression match {
        case binary: BinaryOperator[_, _] => s"$symbol($binary)"
        case expression                   => s"$symbol$expression"
      }
  }

  private[jsonpath] trait BinaryOperator[
      LeftType <: Expression,
      RightType <: Expression
  ] {
    self: Expression =>

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

    protected def compare[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (Int, Int) => Boolean): Boolean = {
      val leftResult = left(evaluator, root, current)
      val rightResult = right(evaluator, root, current)
      leftResult
        .zip(rightResult)
        .flatMap { case (left, right) =>
          left.asString
            .zip(right.asString)
            .map { case (l, r) => l compareTo r }
            .orElse {
              left.asNumber
                .zip(right.asNumber)
                .map { case (l, r) => l compareTo r }
            }
        }
        .fold(this match {
          case equality: IncludesEqualityCheck =>
            equality.equalityCheck(leftResult, rightResult)
          case _ => false
        })(f(_, 0))
    }
  }

  sealed trait IncludesEqualityCheck extends Comparator {

    def equalityCheck[Json](
        left: Option[Json],
        right: Option[Json]
    ): Boolean =
      left -> right match {
        case None -> None       => true
        case Some(l) -> Some(r) => l == r
        case _                  => false
      }
  }

  sealed trait Literal extends Expression with ValueType {

    def asJson[Json: Braid]: Json

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] = Some(asJson[Json])
  }

  case object LiteralNull extends Literal {

    override def asJson[Json: Braid]: Json =
      Braid[Json].Null

    override def toString: String = "null"
  }

  final case class LiteralString(value: String) extends Literal {

    override def asJson[Json: Braid]: Json =
      Braid[Json].fromString(value)

    override def toString: String =
      s"""\"${StringEscapes.escapeDoubleQuotes(value)}\""""
  }

  final case class LiteralNumber(value: BigDecimal) extends Literal {

    def asJson[Json: Braid]: Json =
      Braid[Json].fromBigDecimal(value)

    override def toString: String = value.toString
  }

  object LiteralNumber {

    def apply(int: Int): LiteralNumber =
      LiteralNumber(BigDecimal(int))

    def apply(long: Long): LiteralNumber =
      LiteralNumber(BigDecimal(long))

    def apply(double: Double): LiteralNumber =
      LiteralNumber(BigDecimal(double))

    def apply(float: Float): LiteralNumber =
      LiteralNumber(BigDecimal.decimal(float))
  }

  final case class LiteralBoolean(value: Boolean) extends Literal {

    def asJson[Json: Braid]: Json =
      Braid[Json].fromBoolean(value)

    override def toString: String = value.toString
  }

  final case class JsonPathValue(path: JsonPath.SingularQuery)
      extends Expression
      with NodesType {

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Node[Json]] =
      evaluator
        .evaluate(path, root, Some(current))

    def value: ValueType =
      ValueType.jsonPathValueToValueType(this)

    override def toString: String =
      path.toString
  }

  final case class JsonPathNodes(path: JsonPath)
      extends Expression
      with NodesType {

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Node[Json]] =
      evaluator
        .evaluate(path, root, Some(current))

    override def toString: String =
      path.toString
  }

  final case class Not(expression: LogicalType)
      extends UnaryOperator[LogicalType]
      with LogicalType {

    override def symbol: String = "!"

    override def apply[Json: Braid](
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

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      equalityCheck(
        left(evaluator, root, current),
        right(evaluator, root, current)
      )
  }

  final case class NotEqual(left: ValueType, right: ValueType)
      extends Comparator
      with IncludesEqualityCheck {

    override def symbol: String = "!="

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      !equalityCheck(
        left(evaluator, root, current),
        right(evaluator, root, current)
      )
  }

  final case class GreaterThan(left: ValueType, right: ValueType)
      extends Comparator {

    override val symbol: String = ">"

    override def apply[Json: Braid](
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

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ >= _)
  }

  final case class LessThan(left: ValueType, right: ValueType)
      extends Comparator {

    override val symbol: String = "<"

    override def apply[Json: Braid](
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

    override def apply[Json: Braid](
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

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) &&
        right(evaluator, root, current)

    override def toString: String =
      s"${left match {
        case or: Or => s"($or)"
        case value  => value.toString
      }} $symbol ${right match {
        case and: And                    => and.toString
        case other: BinaryOperator[_, _] => s"($other)"
        case value                       => value.toString
      }}"
  }

  final case class Or(left: LogicalType, right: LogicalType)
      extends BinaryOperator[LogicalType, LogicalType]
      with LogicalType {

    override def symbol: String = "||"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      left(evaluator, root, current) ||
        right(evaluator, root, current)

    override def toString: String =
      s"$left $symbol ${right match {
        case or: Or                      => or.toString
        case and: And                    => and.toString
        case other: BinaryOperator[_, _] => s"(${other.toString})"
        case value                       => value.toString
      }}"
  }
}
