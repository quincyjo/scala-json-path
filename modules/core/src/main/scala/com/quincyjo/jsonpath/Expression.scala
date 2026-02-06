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

/** Base trait for all JSON Path expressions.
  *
  * An Expression represents a computation that can be evaluated against a JSON
  * document to produce a result. Expressions can be of different types,
  * including:
  *   - [[Expression.ValueType]]: Produces a JSON value.
  *   - [[Expression.LogicalType]]: Produces a logical boolean result, distinct
  *     from a JSON boolean.
  *   - [[Expression.NodesType]]: Produces a list of JSON nodes.
  *
  * Expressions can be composed using various operators to create complex
  * queries.
  */
sealed trait Expression {

  /** Attempts to coerce this expression to the specified type.
    *
    * Example:
    * {{{
    *   val expr: Expression = LiteralBoolean(true)
    *   val logical: Validated[String, LogicalType] = expr.as[LogicalType]
    * }}}
    *
    * @tparam T
    *   The target expression type (must have an implicit Coercible instance)
    * @return
    *   A Validated containing the coerced expression or an error message
    */
  def as[T <: Expression: Expression.Coercible]: Validated[String, T] =
    Expression.coerceTo[T](this)
}

/** Companion object for the Expression trait, providing common expressions and
  * type classes.
  */
object Expression {

  /** The JSON `null` literal. */
  final val Null = LiteralNull

  /** The JSON boolean `true` literal. */
  final val True = LiteralBoolean(true)

  /** The JSON boolean `false` literal. */
  final val False = LiteralBoolean(false)

  /** Type class for safely coercing between different expression types.
    *
    * @tparam Type
    *   The target type to coerce to
    */
  sealed trait Coercible[Type] extends (Expression => Validated[String, Type]) {

    def coerce(expression: Expression): Validated[String, Type] =
      apply(expression)
  }

  /** Safely coerce an expression to a specific type.
    *
    * @param expression
    *   The expression to coerce
    * @tparam Type
    *   The target type to coerce to
    * @return
    *   A Validated containing the coerced expression or an error message
    */
  def coerceTo[Type <: Expression: Coercible](
      expression: Expression
  ): Validated[String, Type] =
    implicitly[Coercible[Type]].coerce(expression)

  /** Represents an expression that evaluates to a JSON value or nothing.
    *
    * ValueTypes can be used in comparisons and operations that work with JSON
    * values. They can be combined using various operators to create more
    * complex expressions.
    */
  trait ValueType extends Expression {

    /** Evaluate this expression against a JSONPath evaluation context.
      *
      * @param evaluator
      *   The evaluator to use for JSON Path evaluation
      * @param root
      *   The root document of the JSONPath evaluation
      * @param current
      *   The current node being evaluated
      * @tparam Json
      *   The JSON AST type
      * @return
      *   An Option containing the resulting JSON value, or None if the value is
      *   undefined
      */
    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json]

    /** Compare this value type to another value type for equality.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   An Equal expression representing the comparison
      */
    def isEqualTo(that: ValueType): Equal =
      Equal(this, that)

    /** Alias for [[#isEqualTo]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   An Equal expression representing the comparison
      * @see
      *   [[#isEqualTo]]
      */
    def ===(that: ValueType): Equal =
      isEqualTo(that)

    /** Compare this value type to another value type for inequality.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A NotEqual expression representing the comparison
      */
    def isNotEqualTo(that: ValueType): NotEqual =
      NotEqual(this, that)

    /** Alias for [[#isNotEqualTo]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A NotEqual expression representing the comparison
      * @see
      *   [[#isNotEqualTo]]
      */
    def =!=(that: ValueType): NotEqual =
      isNotEqualTo(that)

    /** Compare this value type to another value type for greater than.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A GreaterThan expression representing the comparison
      */
    def isGreaterThan(that: ValueType): GreaterThan =
      GreaterThan(this, that)

    /** Alias for [[#isGreaterThan]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A GreaterThan expression representing the comparison
      * @see
      *   [[#isGreaterThan]]
      */
    def >(that: ValueType): GreaterThan =
      isGreaterThan(that)

    /** Compare this value type to another value type for greater than or equal
      * to.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A GreaterThanOrEqualTo expression representing the comparison
      */
    def isGreaterThanOrEqualTo(that: ValueType): GreaterThanOrEqualTo =
      GreaterThanOrEqualTo(this, that)

    /** Alias for [[#isGreaterThanOrEqualTo]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A GreaterThanOrEqualTo expression representing the comparison
      * @see
      *   [[#isGreaterThanOrEqualTo]]
      */
    def >=(that: ValueType): GreaterThanOrEqualTo =
      isGreaterThanOrEqualTo(that)

    /** Compare this value type to another value type for less than.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A LessThan expression representing the comparison
      */
    def isLessThan(that: ValueType): LessThan =
      LessThan(this, that)

    /** Alias for [[#isLessThan]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A LessThan expression representing the comparison
      * @see
      *   [[#isLessThan]]
      */
    def <(that: ValueType): LessThan =
      isLessThan(that)

    /** Compare this value type to another value type for less than or equal to.
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A LessThanOrEqualTo expression representing the comparison
      */
    def isLessThanOrEqualTo(that: ValueType): LessThanOrEqualTo =
      LessThanOrEqualTo(this, that)

    /** Alias for [[#isLessThanOrEqualTo]]
      *
      * @param that
      *   The value type to compare to
      * @return
      *   A LessThanOrEqualTo expression representing the comparison
      * @see
      *   [[#isLessThanOrEqualTo]]
      */
    def <=(that: ValueType): LessThanOrEqualTo =
      isLessThanOrEqualTo(that)
  }

  /** Companion object for ValueType providing type class instances and
    * utilities.
    */
  object ValueType {

    implicit val coerceToValueType: Expression.Coercible[ValueType] =
      new Expression.Coercible[ValueType] {

        override def apply(
            expression: Expression
        ): Validated[String, ValueType] =
          ValueType.coerce(expression)
      }

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

  /** Represents a logical expression that evaluates to either logical true or
    * logical false.
    *
    * LogicalTypes can be combined using logical operators (&&, ||, !) to create
    * complex boolean expressions for filtering and conditions.
    */
  trait LogicalType extends Expression {

    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean

    /** Create a logical AND with this and the given value.
      *
      * @param that
      *   The value to AND with
      * @return
      *   A And expression representing the AND operation
      */
    def &&(that: LogicalType): And =
      And(this, that)

    /** Create a logical OR with this and the given value.
      *
      * @param that
      *   The value to OR with
      * @return
      *   A Or expression representing the OR operation
      */
    def ||(that: LogicalType): Or =
      Or(this, that)

    /** Create a logical NOT with this value.
      *
      * @return
      *   A Not expression representing the NOT operation
      */
    def unary_! : Not =
      Not(this)
  }

  /** Companion object for LogicalType providing type class instances and
    * utilities.
    */
  object LogicalType {

    implicit val coerceToLogicalType: Expression.Coercible[LogicalType] =
      new Expression.Coercible[LogicalType] {

        override def apply(
            expression: Expression
        ): Validated[String, LogicalType] =
          LogicalType.coerce(expression)
      }

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

  /** Represents an expression that evaluates to a list of JSONPath nodes.
    *
    * NodesTypes are typically the result of JSON Path queries and can be
    * converted to LogicalType (checking for existence) or ValueType if it is a
    * singular query.
    */
  trait NodesType extends Expression {

    def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): List[Node[Json]]

    /** Convenience method for creating a LogicalType from a NodesType.
      *
      * @return
      *   A logical type as existence check.
      */
    def exists: LogicalType =
      LogicalType.nodesTypeToLogicalType(this)
  }

  /** Companion object for NodesType providing type class instances and
    * utilities.
    */
  object NodesType {

    implicit val coerceToNodesType: Expression.Coercible[NodesType] =
      new Expression.Coercible[NodesType] {

        override def apply(
            expression: Expression
        ): Validated[String, NodesType] =
          NodesType.coerce(expression)
      }

    def coerce(expression: Expression): Validated[String, NodesType] =
      expression match {
        case nodesType: NodesType =>
          Validated.valid(nodesType)
        case _: LogicalType =>
          Validated.invalid("LogicalType cannot be coerced to NodesType.")
        case _: ValueType =>
          Validated.invalid("ValueType cannot be coerced to NodesType.")
      }
  }

  /** Base trait for literal JSON values.
    *
    * Literals are atomic JSON values that can be used in expressions.
    */
  sealed trait Literal extends Expression with ValueType {

    /** Convert this literal to a JSON value of the given JSON type.
      *
      * @tparam Json
      *   The JSON type to convert to.
      */
    def asJson[Json: Braid]: Json

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] = Some(asJson[Json])
  }

  /** The JSON `null` literal value. */
  case object LiteralNull extends Literal {

    override def asJson[Json: Braid]: Json =
      Braid[Json].Null

    override def toString: String = "null"
  }

  /** A JSON string literal value.
    *
    * @param value
    *   The string value
    */
  final case class LiteralString(value: String) extends Literal {

    override def asJson[Json: Braid]: Json =
      Braid[Json].fromString(value)

    override def toString: String =
      s"""\"${StringEscapes.escapeDoubleQuotes(value)}\""""
  }

  /** A JSON number literal value.
    *
    * @param value
    *   The numeric value as a BigDecimal
    */
  final case class LiteralNumber(value: BigDecimal) extends Literal {

    def asJson[Json: Braid]: Json =
      Braid[Json].fromBigDecimal(value)

    override def toString: String = value.toString
  }

  /** Factory methods for creating numeric literals from different numeric
    * types.
    */
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

  /** A JSON boolean literal value.
    *
    * @param value
    *   The boolean value
    */
  final case class LiteralBoolean(value: Boolean) extends Literal {

    def asJson[Json: Braid]: Json =
      Braid[Json].fromBoolean(value)

    override def toString: String = value.toString
  }

  /** An expression that evaluates a JSON Path query that must return at most a
    * single value via a singular query.
    *
    * @param path
    *   The JSON Path query to evaluate
    */
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

  /** An expression that evaluates a JSON Path query that may return multiple
    * nodes.
    *
    * @param path
    *   The JSON Path query to evaluate
    */
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

  /** Logical NOT operation.
    *
    * @param expression
    *   The expression to negate
    */
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

  /** Equality comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class Equal(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator
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

  /** Inequality comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class NotEqual(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator
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

  /** Greater than comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class GreaterThan(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator {

    override val symbol: String = ">"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ > _)
  }

  /** Greater than or equal to comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class GreaterThanOrEqualTo(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator
      with IncludesEqualityCheck {

    override val symbol: String = ">="

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ >= _)
  }

  /** Less than comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class LessThan(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator {

    override val symbol: String = "<"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ < _)
  }

  /** Less than or equal to comparison between two ValueType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
  final case class LessThanOrEqualTo(left: ValueType, right: ValueType)
      extends BinaryOperator[ValueType, ValueType]
      with LogicalType
      with Comparator
      with IncludesEqualityCheck {

    override val symbol: String = "<="

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Boolean =
      compare(evaluator, root, current)(_ <= _)
  }

  /** Logical and (&&) between two LogicalType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
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

  /** Logical or (||) between two LogicalType expressions.
    *
    * @param left
    *   The left-hand side expression
    * @param right
    *   The right-hand side expression
    */
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

  private[jsonpath] sealed trait Comparator {
    self: BinaryOperator[ValueType, ValueType] =>

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

  private[jsonpath] sealed trait IncludesEqualityCheck {
    comparator: Comparator =>

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
}
