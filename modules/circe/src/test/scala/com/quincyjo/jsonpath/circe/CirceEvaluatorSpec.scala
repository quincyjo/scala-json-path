/*
 * Copyright 2023 Typelevel
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

package com.quincyjo.jsonpath.circe

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonPath._
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CirceEvaluatorSpec extends AnyFlatSpecLike with Matchers {

  private val apple = Json.obj(
    "label" -> "Apple".asJson,
    "price" -> 2.asJson,
    "quantity" -> 15.asJson
  )
  private val banana = Json.obj(
    "label" -> "Banana".asJson,
    "price" -> 1.asJson,
    "quantity" -> 23.asJson
  )
  private val json = Json.obj(
    "products" -> Json.arr(
      apple,
      banana,
      Json.obj(
        "label" -> "Dinner Set".asJson,
        "price" -> 30.asJson,
        "quantity" -> 2.asJson
      )
    )
  )

  "evaluate" should "behave" in {
    val jsonPath = $ / "products" / Filter(
      LessThanOrEqualTo(
        JsonPathValue(`@` / "price"),
        JsonNumber(10)
      )
    )

    CirceEvaluator.evaluate(
      jsonPath,
      json
    ) should contain theSameElementsAs Seq(
      apple,
      banana
    )
  }
}
