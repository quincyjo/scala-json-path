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

import com.quincyjo.jsonpath.Expression.{JsonPathValue, LiteralString}
import com.quincyjo.jsonpath.JsonBean
import com.quincyjo.jsonpath.JsonBean.JsonBeanEvaluator
import com.quincyjo.jsonpath.JsonPath._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class LengthSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  "Length" should "return the length of strings" in {
    val cases = Table(
      "given string" -> "expected length",
      "" -> 0,
      "foo" -> 3,
      "foobar" -> 6
    )

    forAll(cases) { case (givenString, expectedLength) =>
      Length(LiteralString(givenString))(
        JsonBeanEvaluator,
        JsonBean.Null,
        JsonBean.Null
      ).value should be(JsonBean.number(expectedLength))
    }
  }

  it should "return the length of arrays" in {
    val givenJson = JsonBean.obj(
      "five" -> JsonBean.arr(1, 2, 3, 4, 5),
      "one" -> JsonBean.arr(1),
      "zero" -> JsonBean.arr()
    )

    val cases = Table(
      "given query" -> "expected length",
      $ / "zero" -> 0,
      $ / "one" -> 1,
      $ / "five" -> 5
    )

    forAll(cases) { case (givenQuery, expectedLength) =>
      Length(JsonPathValue(givenQuery))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ).value should be(JsonBean.number(expectedLength))
    }
  }

  it should "return the length of objects" in {
    val givenJson = JsonBean.obj(
      "five" -> JsonBean.obj(
        "one" -> "bar",
        "two" -> "bar",
        "three" -> "bar",
        "four" -> "bar",
        "five" -> "bar"
      ),
      "one" -> JsonBean.obj("foo" -> "bar"),
      "zero" -> JsonBean.obj()
    )

    val cases = Table(
      "given query" -> "expected length",
      $ / "zero" -> 0,
      $ / "one" -> 1,
      $ / "five" -> 5
    )

    forAll(cases) { case (givenQuery, expectedLength) =>
      Length(JsonPathValue(givenQuery))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ).value should be(JsonBean.number(expectedLength))
    }
  }
}
