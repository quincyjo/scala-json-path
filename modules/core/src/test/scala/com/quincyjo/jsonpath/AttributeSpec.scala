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

import com.quincyjo.jsonpath.JsonPath.Attribute
import org.scalatest.{EitherValues, LoneElement}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AttributeSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with EitherValues
    with TableDrivenPropertyChecks {

  "isSimple" should "determine if the name is dot-chainable" in {
    val cases = Table(
      "right" -> "expected",
      "foobar" -> true,
      "abc123" -> true,
      "1" -> false,
      "123abc" -> false
    )

    forAll(cases) { case (name, expected) =>
      Attribute(name).isSimple should be(expected)
    }
  }

  "toString" should "be quoted in single quotes" in {
    Attribute("\"foobar\"").toString should be("\'\"foobar\"\'")
  }

  it should "encode things" in {
    val cases = Table(
      "input" -> "expected",
      "foobar's" -> "'foobar\\'s'"
    )

    forAll(cases) { (input, expected) =>
      Attribute(input).toString should be(expected)
    }
  }
}
