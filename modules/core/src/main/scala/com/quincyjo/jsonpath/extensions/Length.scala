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
import com.quincyjo.braid.implicits._
import com.quincyjo.jsonpath.Expression.ValueType
import com.quincyjo.jsonpath.parser.{JsonPathParser, WithExtension}
import com.quincyjo.jsonpath.{Expression, JsonPathEvaluator}

/** Function that returns the length of the given value. If the value is a
  * string, then the length of the string is returned. If the value is an array
  * or object, then the number of elements in the array or values in the object
  * is returned. If the value is not on of those three, then nothing is
  * returned.
  * @param value
  *   The value to get the length of.
  */
final case class Length(value: ValueType)
    extends FunctionExtension[ValueType]
    with ValueType {

  override val name: String = "length"

  override val args: List[Expression] = List(value)

  override def apply[Json: Braid](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Option[Json] = value(evaluator, root, current)
    .flatMap { json =>
      json
        .arrayOrObject[Option[Int]](
          json.asString.map(_.length),
          arr => Some(arr.size),
          obj => Some(obj.size)
        )
        .map(Braid[Json].fromInt)
    }
}

object Length {

  val extension: Extension[ValueType, Length] =
    Extension("length")(Length.apply)

  trait LengthExtension extends WithExtension { self: JsonPathParser =>

    addExtension(Length.extension)
  }
}
