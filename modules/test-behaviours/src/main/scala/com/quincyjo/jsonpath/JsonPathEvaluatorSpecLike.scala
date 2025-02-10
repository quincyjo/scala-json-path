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

import com.quincyjo.braid.Braid
import com.quincyjo.jsonpath.JsonPath.Wildcard
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait JsonPathEvaluatorSpecLike
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  def basicEvaluations[Json](
      evaluator: JsonPathEvaluator[Json]
  )(implicit braid: Braid[Json]): Unit = {
    val firstName = "Jane"
    val lastName = "Doe"
    val streetAddress = "123 Sesame St"
    val city = "Cityville"
    val country = "ABC"
    val mobilePhoneType = "mobile"
    val mobilePhoneNumber = "555-111-2222"
    val homePhoneType = "home"
    val homePhoneNumber = "555-333-4444"
    val address = braid.obj(
      "streetAddress" -> braid.fromString(streetAddress),
      "city" -> braid.fromString(city),
      "country" -> braid.fromString(country)
    )
    val mobilePhone = braid.obj(
      "type" -> braid.fromString(mobilePhoneType),
      "number" -> braid.fromString(mobilePhoneNumber)
    )
    val homePhone = braid.obj(
      "type" -> braid.fromString(homePhoneType),
      "number" -> braid.fromString(homePhoneNumber)
    )
    val phoneNumbers = braid.arr(mobilePhone, homePhone)
    val json = braid.obj(
      "firstName" -> braid.fromString(firstName),
      "lastName" -> braid.fromString(lastName),
      "address" -> address,
      "phoneNumbers" -> phoneNumbers
    )

    "evaluate" should "return a list of the matching values" in {
      val cases = Table[JsonPath, List[Json]](
        "jsonPath" -> "expected",
        JsonPath.$ / "firstName" -> List(braid.fromString(firstName)),
        JsonPath.$ / "lastName" -> List(braid.fromString(lastName)),
        JsonPath.$ / "address" -> List(address),
        JsonPath.$ / "phoneNumbers" / Wildcard / "number" -> List(
          braid.fromString(mobilePhoneNumber),
          braid.fromString(homePhoneNumber)
        ),
        JsonPath.$ */ Wildcard -> List(
          braid.fromString(firstName),
          braid.fromString(lastName),
          address,
          phoneNumbers,
          braid.fromString(streetAddress),
          braid.fromString(city),
          braid.fromString(country),
          mobilePhone,
          homePhone,
          braid.fromString(mobilePhoneType),
          braid.fromString(mobilePhoneNumber),
          braid.fromString(homePhoneType),
          braid.fromString(homePhoneNumber)
        )
      )

      forAll(cases) { case (jsonPath, expected) =>
        val results = evaluator.evaluate(jsonPath, json)
        results.map(_.value) should contain theSameElementsAs expected
        results.foreach { case Node(location, value) =>
          evaluator.singular(location, json).value.value should be(value)
        }
      }
    }

    it should "handle basic expressions" in {

      val apple = braid.obj(
        "label" -> braid.fromString("Apple"),
        "price" -> braid.fromInt(2),
        "quantity" -> braid.fromInt(15)
      )
      val banana = braid.obj(
        "label" -> braid.fromString("Banana"),
        "price" -> braid.fromInt(1),
        "quantity" -> braid.fromInt(23)
      )
      val dinnerSet = braid.obj(
        "label" -> braid.fromString("Dinner Set"),
        "price" -> braid.fromInt(30),
        "quantity" -> braid.fromInt(2)
      )
      val json = braid.obj(
        "products" -> braid.arr(
          apple,
          banana,
          dinnerSet
        )
      )

      val jsonPath = JsonPath.$ / "products" / JsonPath.Filter(
        Expression.LessThanOrEqualTo(
          Expression.JsonPathValue(JsonPath.`@` / "price"),
          Expression.LiteralNumber(10)
        )
      )

      val results = evaluator.evaluate(jsonPath, json)
      results.map(_.value) should contain theSameElementsAs Seq(
        apple,
        banana
      )
      results.foreach { case Node(location, value) =>
        evaluator.singular(location, json).value.value should be(value)
      }
    }
  }
}
