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

import com.quincyjo.jsonpath.Expression.JsonPathNodes
import com.quincyjo.jsonpath.JsonBean
import com.quincyjo.jsonpath.JsonBean.JsonBeanEvaluator
import com.quincyjo.jsonpath.JsonPath._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CountSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  "Count" should "count the number of nodes returned by its query" in {
    val givenJson = JsonBean.obj(
      "foo" -> JsonBean.arr(1, 2, 3, 4, 5),
      "bar" -> JsonBean.arr(1)
    )
    val cases = Table(
      "jsonPath" -> "expected count",
      $ / "foo" -> 1,
      $ / "foo" / Wildcard -> 5,
      $ / "foo" / Union(0, 1) -> 2,
      $ / "foo" / Slice.take(3) -> 3,
      $ / "bar" / Slice.take(3) -> 1,
      $ */ Wildcard -> 8
    )

    forAll(cases) { case (query, expectedCount) =>
      Count(JsonPathNodes(query))(
        JsonBeanEvaluator,
        givenJson,
        givenJson
      ).value should be(JsonBean.number(expectedCount))
    }
  }
}
