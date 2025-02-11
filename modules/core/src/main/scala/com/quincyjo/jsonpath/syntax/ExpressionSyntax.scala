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

package com.quincyjo.jsonpath.syntax

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonPath

trait ExpressionSyntax {

  implicit def toValueType[T](value: T)(implicit
      magnet: ExpressionSyntax.ValueTypeMagnet[T]
  ): ValueType =
    magnet(value)

  implicit def toLogicalType[T](value: T)(implicit
      magnet: ExpressionSyntax.LogicalTypeMagnet[T]
  ): LogicalType =
    magnet(value)

  implicit def toNodesType[T](value: T)(implicit
      magnet: ExpressionSyntax.NodesTypeMagnet[T]
  ): NodesType =
    magnet(value)
}

object ExpressionSyntax extends ExpressionSyntax {

  sealed trait ValueTypeMagnet[-T] {

    def apply(t: T): ValueType
  }

  object ValueTypeMagnet {

    implicit case object StringMagnet extends ValueTypeMagnet[String] {

      override def apply(t: String): ValueType = LiteralString(t)
    }

    implicit case object BooleanMagnet extends ValueTypeMagnet[Boolean] {

      override def apply(t: Boolean): ValueType = LiteralBoolean(t)
    }

    implicit case object BigDecimalMagnet extends ValueTypeMagnet[BigDecimal] {

      override def apply(t: BigDecimal): ValueType = LiteralNumber(t)
    }

    implicit case object IntMagnet extends ValueTypeMagnet[Int] {

      override def apply(t: Int): ValueType = LiteralNumber(t)
    }

    implicit case object LongMagnet extends ValueTypeMagnet[Long] {

      override def apply(t: Long): ValueType = LiteralNumber(t)
    }

    implicit case object DoubleMagnet extends ValueTypeMagnet[Double] {

      override def apply(t: Double): ValueType = LiteralNumber(t)
    }

    implicit case object FloatMagnet extends ValueTypeMagnet[Float] {

      override def apply(t: Float): ValueType = LiteralNumber(t)
    }

    implicit case object SingularQueryMagnet
        extends ValueTypeMagnet[JsonPath.SingularQuery] {

      override def apply(t: JsonPath.SingularQuery): ValueType =
        ValueType.jsonPathValueToValueType(JsonPathValue(t))
    }
  }

  sealed trait LogicalTypeMagnet[-T] {

    def apply(t: T): LogicalType
  }

  object LogicalTypeMagnet {

    implicit case object JsonPathMagnet extends LogicalTypeMagnet[JsonPath] {

      override def apply(t: JsonPath): LogicalType =
        LogicalType.nodesTypeToLogicalType(JsonPathNodes(t))
    }
  }

  sealed trait NodesTypeMagnet[-T] {

    def apply(t: T): NodesType
  }

  object NodesTypeMagnet {

    implicit case object JsonPathMagnet extends NodesTypeMagnet[JsonPath] {

      override def apply(t: JsonPath): NodesType =
        JsonPathNodes(t)
    }
  }
}
