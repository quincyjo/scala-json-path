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

import com.quincyjo.jsonpath.Expression.{JsonPathNodes, JsonPathValue}
import com.quincyjo.jsonpath.JsonBean
import com.quincyjo.jsonpath.JsonBean.JsonBeanEvaluator
import com.quincyjo.jsonpath.JsonPath._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ValueSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  private val givenJson = JsonBean.obj(
    "foo" -> JsonBean.arr(1, 2, 3, 4, 5),
    "bar" -> JsonBean.arr(1)
  )

  "Value" should "return the value of the node of defined singular queries" in {
    val cases = Table(
      "given query" -> "expected value",
      $ / "foo" -> JsonBean.arr(1, 2, 3, 4, 5),
      $ / "bar" -> JsonBean.arr(1),
      $ / "foo" / 0 -> JsonBean.number(1)
    )

    forAll(cases) { case (givenQuery, expectedValue) =>
      Value(JsonPathValue(givenQuery))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ).value should be(expectedValue)
    }
  }

  it should "return None if there are no nodes" in {
    val cases = Table(
      "given query",
      $ / "baz",
      $ / "baz" / 0,
      $ / "foo" / 6
    )

    forAll(cases) { givenQuery =>
      Value(JsonPathValue(givenQuery))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ) should be(empty)
    }
  }

  // Note that we can only test definitive values with ordered values, EG arrays.
  it should "return a value from queries with multiple nodes`" in {
    val cases = Table(
      "given query" -> "expected value",
      $ / "foo" / Wildcard -> JsonBean.number(1),
      $ / "foo" / Slice.take(3) -> JsonBean.number(1)
    )

    forAll(cases) { case (givenQuery, expectedValue) =>
      Value(JsonPathNodes(givenQuery))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ).value should be(expectedValue)
    }
  }
}
