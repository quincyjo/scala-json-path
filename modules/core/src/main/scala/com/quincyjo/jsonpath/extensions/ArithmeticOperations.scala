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

package com.quincyjo.jsonpath.extensions

import com.quincyjo.braid.Braid
import com.quincyjo.braid.implicits.toBraidedJson
import com.quincyjo.braid.operations.implicits.toJsonOperationOps
import com.quincyjo.jsonpath.Expression.{BinaryOperator, ValueType}
import com.quincyjo.jsonpath.JsonPathEvaluator
import com.quincyjo.jsonpath.parser.JsonPathParser

/** Mix-in to provide arithmetic operations to a JSON path parser.
  */
trait ArithmeticOperations { self: JsonPathParser =>

  override val enableArithmeticOperators: Boolean = true
}

object ArithmeticOperations {

  sealed trait ArithmeticOperator
      extends BinaryOperator[ValueType, ValueType]
      with ValueType {

    protected def arithmetic[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    )(f: (BigDecimal, BigDecimal) => BigDecimal): Option[Json] =
      left(evaluator, root, current)
        .flatMap(_.coerceToNumber)
        .zip(right(evaluator, root, current).flatMap(_.coerceToNumber))
        .map { case (left, right) =>
          Braid[Json].fromBigDecimal(f(left, right))
        }
  }

  private[jsonpath] sealed trait HighPriority extends ArithmeticOperator {

    protected def serializeRight: String = right match {
      case other: BinaryOperator[_, _] => s"(${other.toString})"
      case value                       => value.toString
    }

    override def toString: String =
      s"${left match {
        case lowPriority: LowPriority => s"($lowPriority)"
        case value                    => value.toString
      }} $symbol $serializeRight"
  }

  private[jsonpath] sealed trait LowPriority extends ArithmeticOperator {

    protected def serializeRight: String = right match {
      case highPriority: HighPriority  => highPriority.toString
      case other: BinaryOperator[_, _] => s"(${other.toString})"
      case value                       => value.toString
    }

    override def toString: String =
      s"$left $symbol $serializeRight"
  }

  /** Loose summation arithmetic operation. If both operands are either a number
    * or null, the result is a number. If either side is the special result
    * Nothing, then Nothing is the result. Otherwise, the two sides are coerced
    * to strings and concatenated.
    * @param left
    *   The left hand side.
    * @param right
    *   The right hand side.
    */
  final case class Plus(left: ValueType, right: ValueType) extends LowPriority {

    override def symbol: String = "+"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] = {
      left(evaluator, root, current)
        .zip(right(evaluator, root, current))
        .map { case (leftResult, rightResult) =>
          if (
            (leftResult.isNumber || leftResult.isNull) &&
            (rightResult.isNumber || rightResult.isNull)
          )
            Braid[Json].fromBigDecimal(
              leftResult.coerceToNumber.getOrElse(BigDecimal(0)) +
                rightResult.coerceToNumber.getOrElse(BigDecimal(0))
            )
          else
            Braid[Json].fromString(
              leftResult.coerceToString concat rightResult.coerceToString
            )
        }
    }

    override protected def serializeRight: String = right match {
      case plus: Plus => plus.toString
      case _          => super.serializeRight
    }
  }

  /** Subtraction arithmetic operation. If either operand is the special result
    * Nothing, then Nothing is the result. Otherwise, the two sides are coerced
    * to numbers and arithmetically subtracted.
    * @param left
    *   The left hand side.
    * @param right
    *   The right hand side.
    */
  final case class Minus(left: ValueType, right: ValueType)
      extends LowPriority {

    override def symbol: String = "-"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ - _)
  }

  /** Multiplication arithmetic operation. If either operand is the special
    * result Nothing, then Nothing is the result. Otherwise, the two sides are
    * coerced to numbers and the left hand result is divided by the right hand
    * result.
    * @param left
    *   The left hand side.
    * @param right
    *   The right hand side.
    */
  final case class Divide(left: ValueType, right: ValueType)
      extends HighPriority {

    override def symbol: String = "/"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ / _)
  }

  /** Multiplication arithmetic operation. If either operand is the special
    * result Nothing, then Nothing is the result. Otherwise, the two sides are
    * coerced to numbers and arithmetically multiplied.
    * @param left
    *   The left hand side.
    * @param right
    *   The right hand side.
    */
  final case class Multiply(left: ValueType, right: ValueType)
      extends HighPriority {

    override def symbol: String = "*"

    override def apply[Json: Braid](
        evaluator: JsonPathEvaluator[Json],
        root: Json,
        current: Json
    ): Option[Json] =
      arithmetic(evaluator, root, current)(_ * _)

    override protected def serializeRight: String = right match {
      case multiply: Multiply => multiply.toString
      case _                  => super.serializeRight
    }
  }
}
