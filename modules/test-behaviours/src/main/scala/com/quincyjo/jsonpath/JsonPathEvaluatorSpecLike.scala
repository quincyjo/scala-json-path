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

import com.quincyjo.jsonpath.JsonPath.{RecursiveDescent, Wildcard}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait JsonPathEvaluatorSpecLike
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks {

  def basicEvaluations[Json](
      evaluator: JsonPathEvaluator[Json]
  )(implicit jsonSupport: JsonSupport[Json]): Unit = {
    val firstName = "Jane"
    val lastName = "Doe"
    val streetAddress = "123 Sesame St"
    val city = "Cityville"
    val country = "ABC"
    val mobilePhoneType = "mobile"
    val mobilePhoneNumber = "555-111-2222"
    val homePhoneType = "home"
    val homePhoneNumber = "555-333-4444"
    val address = jsonSupport.obj(
      "streetAddress" -> jsonSupport.string(streetAddress),
      "city" -> jsonSupport.string(city),
      "country" -> jsonSupport.string(country)
    )
    val mobilePhone = jsonSupport.obj(
      "type" -> jsonSupport.string(mobilePhoneType),
      "number" -> jsonSupport.string(mobilePhoneNumber)
    )
    val homePhone = jsonSupport.obj(
      "type" -> jsonSupport.string(homePhoneType),
      "number" -> jsonSupport.string(homePhoneNumber)
    )
    val phoneNumbers = jsonSupport.arr(mobilePhone, homePhone)
    val json = jsonSupport.obj(
      "firstName" -> jsonSupport.string(firstName),
      "lastName" -> jsonSupport.string(lastName),
      "address" -> address,
      "phoneNumbers" -> phoneNumbers
    )

    "evaluate" should "foo" in {
      val cases = Table[JsonPath, List[Json]](
        "jsonPath" -> "expected",
        JsonPath.$ / "firstName" -> List(jsonSupport.string(firstName)),
        JsonPath.$ / "lastName" -> List(jsonSupport.string(lastName)),
        JsonPath.$ / "address" -> List(address),
        JsonPath.$ / "phoneNumbers" / Wildcard / "number" -> List(
          jsonSupport.string(mobilePhoneNumber),
          jsonSupport.string(homePhoneNumber)
        ),
        JsonPath.$ */ Wildcard -> List(
          jsonSupport.string(firstName),
          jsonSupport.string(lastName),
          address,
          phoneNumbers,
          jsonSupport.string(streetAddress),
          jsonSupport.string(city),
          jsonSupport.string(country),
          mobilePhone,
          homePhone,
          jsonSupport.string(mobilePhoneType),
          jsonSupport.string(mobilePhoneNumber),
          jsonSupport.string(homePhoneType),
          jsonSupport.string(homePhoneNumber)
        )
      )

      forAll(cases) { case (jsonPath, expected) =>
        evaluator.evaluate(
          jsonPath,
          json
        ) should contain theSameElementsAs expected
      }
    }

    it should "handle basic expressions" in {

      val apple = jsonSupport.obj(
        "label" -> jsonSupport.string("Apple"),
        "price" -> jsonSupport.number(2),
        "quantity" -> jsonSupport.number(15)
      )
      val banana = jsonSupport.obj(
        "label" -> jsonSupport.string("Banana"),
        "price" -> jsonSupport.number(1),
        "quantity" -> jsonSupport.number(23)
      )
      val dinnerSet = jsonSupport.obj(
        "label" -> jsonSupport.string("Dinner Set"),
        "price" -> jsonSupport.number(30),
        "quantity" -> jsonSupport.number(2)
      )
      val json = jsonSupport.obj(
        "products" -> jsonSupport.arr(
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

      evaluator.evaluate(
        jsonPath,
        json
      ) should contain theSameElementsAs Seq(
        apple,
        banana
      )
    }
  }
}
