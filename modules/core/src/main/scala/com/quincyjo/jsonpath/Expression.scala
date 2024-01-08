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

sealed trait Expression {

  def apply[Json: JsonSupport](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Json
}

object Expression {

  sealed trait UnaryOperator extends Expression {

    def symbol: String

    def expression: Expression

    override def toString: String =
      expression match {
        case binary: BinaryOperator => s"$symbol($binary)"
        case expression             => s"$symbol$expression"
      }
  }

  sealed trait BinaryOperator extends Expression {

    def symbol: String

    def left: Expression

    def right: Expression

    override def toString: String =
      s"$left $symbol ${right match {
        case value: Value => value.toString
        case other        => s"(${other.toString})"
      }}"
  }

  sealed trait Comparator extends BinaryOperator {

    protected def compare[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (Int, Int) => Boolean): Json = {
      val leftResult = left(evaluator, root, current)
      val rightResult = right(evaluator, root, current)
      implicitly[JsonSupport[Json]].boolean(
        leftResult.asString
          .zip(rightResult.asString)
          .map { case (l, r) => l compareTo r }
          .orElse(
            leftResult.asArray
              .zip(rightResult.asArray)
              .map { case (l, r) => l.size compareTo r.size }
          )
          .orElse(
            leftResult.coerceToNumber
              .zip(rightResult.coerceToNumber)
              .map { case (l, r) => l compareTo r }
          )
          .fold(false)(f(_, 0))
      )
    }
  }

  sealed trait Value extends Expression

  sealed trait JsonValue extends Value {

    def asJson[Json: JsonSupport]: Json
  }

  case object JsonNull extends JsonValue {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json = asJson[Json]

    override def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].Null

    override def toString: String = "null"
  }

  final case class JsonString(value: String) extends JsonValue {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json = asJson[Json]

    override def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].string(value)

    override def toString: String =
      s"""\"${value.replace("\"", "\\\"")}\""""
  }

  final case class JsonNumber(value: BigDecimal) extends JsonValue {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json = asJson[Json]

    def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].number(value)

    override def toString: String = value.toString
  }

  final case class JsonBoolean(value: Boolean) extends JsonValue {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json = asJson[Json]

    def asJson[Json: JsonSupport]: Json =
      implicitly[JsonSupport[Json]].boolean(value)

    override def toString: String = value.toString
  }

  final case class JsonPathValue(path: JsonPath) extends Value {

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      evaluator
        .evaluate(path, root, Some(current))
        .headOption
        .getOrElse(implicitly[JsonSupport[Json]].Null)

    override def toString: String =
      path.toString
  }

  final case class Not(expression: Expression) extends UnaryOperator {

    override def symbol: String = "!"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      implicitly[JsonSupport[Json]].boolean(
        !expression(evaluator, root, current).coerceToBoolean
      )
  }

  final case class Equal(left: Expression, right: Expression)
      extends BinaryOperator {

    override def symbol: String = "=="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      implicitly[JsonSupport[Json]].boolean(
        implicitly[JsonSupport[Json]]
          .convertTypes(
            left(evaluator, root, current),
            right(evaluator, root, current)
          )
          .fold(false) { case (l, r) =>
            l == r
          }
      )
  }

  final case class NotEqual(left: Expression, right: Expression)
      extends BinaryOperator {

    override def symbol: String = "!="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      implicitly[JsonSupport[Json]].boolean(
        implicitly[JsonSupport[Json]]
          .convertTypes(
            left(evaluator, root, current),
            right(evaluator, root, current)
          )
          .fold(true) { case (l, r) =>
            l != r
          }
      )
  }

  final case class GreaterThan(left: Expression, right: Expression)
      extends Comparator {

    override val symbol: String = ">"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      compare(evaluator, root, current)(_ > _)
  }

  final case class GreaterThanOrEqualTo(left: Expression, right: Expression)
      extends Comparator {

    override val symbol: String = ">="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      compare(evaluator, root, current)(_ >= _)
  }

  final case class LessThan(left: Expression, right: Expression)
      extends Comparator {

    override val symbol: String = "<"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      compare(evaluator, root, current)(_ < _)
  }

  final case class LessThanOrEqualTo(left: Expression, right: Expression)
      extends Comparator {

    override val symbol: String = "<="

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      compare(evaluator, root, current)(_ <= _)
  }

  final case class And(left: Expression, right: Expression)
      extends BinaryOperator {

    override def symbol: String = "&&"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      implicitly[JsonSupport[Json]].boolean(
        left(evaluator, root, current).coerceToBoolean &&
          right(evaluator, root, current).coerceToBoolean
      )
  }

  final case class Or(left: Expression, right: Expression)
      extends BinaryOperator {

    override def symbol: String = "||"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      implicitly[JsonSupport[Json]].boolean(
        left(evaluator, root, current).coerceToBoolean ||
          right(evaluator, root, current).coerceToBoolean
      )
  }

  sealed trait ArithmeticOperator extends BinaryOperator {

    protected def arithmetic[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (BigDecimal, BigDecimal) => BigDecimal): Json = {
      left(evaluator, root, current).coerceToNumber
        .zip(right(evaluator, root, current).coerceToNumber)
        .map { case (left, right) =>
          implicitly[JsonSupport[Json]].number(f(left, right))
        }
        .getOrElse(implicitly[JsonSupport[Json]].Null)
    }
  }

  final case class Plus(left: Expression, right: Expression)
      extends BinaryOperator {

    override def symbol: String = "+"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json = {
      val leftResult = left(evaluator, root, current)
      val rightResult = right(evaluator, root, current)
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

  final case class Minus(left: Expression, right: Expression)
      extends ArithmeticOperator {

    override def symbol: String = "-"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      arithmetic(evaluator, root, current)(_ - _)
  }

  final case class Divide(left: Expression, right: Expression)
      extends ArithmeticOperator {

    override def symbol: String = "/"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      arithmetic(evaluator, root, current)(_ / _)
  }

  final case class Multiply(left: Expression, right: Expression)
      extends ArithmeticOperator {

    override def symbol: String = "*"

    override def apply[Json: JsonSupport](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Json =
      arithmetic(evaluator, root, current)(_ * _)
  }
}
