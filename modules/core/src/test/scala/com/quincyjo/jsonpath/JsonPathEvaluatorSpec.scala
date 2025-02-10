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

import com.quincyjo.jsonpath.JsonBean._
import com.quincyjo.jsonpath.JsonPath.{Slice, Union, Wildcard}
import org.scalatest.{LoneElement, OptionValues}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathEvaluatorSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with OptionValues
    with TableDrivenPropertyChecks {

  "evaluate" should "select according to the path" in {
    val firstName = "Jane"
    val lastName = "Doe"
    val streetAddress = "123 Sesame St"
    val city = "Cityville"
    val country = "ABC"
    val mobilePhoneType = "mobile"
    val mobilePhoneNumber = "555-111-2222"
    val homePhoneType = "home"
    val homePhoneNumber = "555-333-4444"
    val address = JsonBean.obj(
      "streetAddress" -> streetAddress,
      "city" -> city,
      "country" -> country
    )
    val mobilePhone = JsonBean.obj(
      "type" -> mobilePhoneType,
      "number" -> mobilePhoneNumber
    )
    val homePhone = JsonBean.obj(
      "type" -> homePhoneType,
      "number" -> homePhoneNumber
    )
    val phoneNumbers = JsonBean.arr(mobilePhone, homePhone)
    val json = JsonBean.obj(
      "firstName" -> firstName,
      "lastName" -> lastName,
      "address" -> address,
      "phoneNumbers" -> phoneNumbers
    )

    val cases = Table[JsonPath, List[JsonBean]](
      "jsonPath" -> "expected",
      JsonPath.$ / "firstName" -> List(JsonBean(firstName)),
      JsonPath.$ / "lastName" -> List(JsonBean(lastName)),
      JsonPath.$ / "address" -> List(address),
      JsonPath.$ / "phoneNumbers" / Wildcard / "number" -> List(
        JsonBean(mobilePhoneNumber),
        JsonBean(homePhoneNumber)
      ),
      JsonPath.$ */ Wildcard -> List(
        JsonBean(firstName),
        JsonBean(lastName),
        address,
        phoneNumbers,
        JsonBean(streetAddress),
        JsonBean(city),
        JsonBean(country),
        mobilePhone,
        homePhone,
        JsonBean(mobilePhoneType),
        JsonBean(mobilePhoneNumber),
        JsonBean(homePhoneType),
        JsonBean(homePhoneNumber)
      )
    )

    forAll(cases) { case (jsonPath, expected) =>
      val results =
        JsonBeanEvaluator.evaluate(
          jsonPath,
          json
        )
      results.map(_.value) should contain theSameElementsAs expected
      results.foreach { case Node(location, value) =>
        JsonBeanEvaluator.singular(location, json).value.value should be(value)
      }
    }
  }

  it should "be against the current json if dynamic" in {
    val json = JsonBean.obj(
      "right" -> "fruits",
      "items" -> JsonBean.arr(
        "apple",
        "banana",
        "orange"
      )
    )
    val root = JsonBean.obj(
      "categories" -> JsonBean.arr(json)
    )

    val cases = Table(
      "jsonPath" -> "expected",
      JsonPath.`@` / "right" -> List(JsonBean("fruits")),
      JsonPath.`@` / "items" / 0 -> List(JsonBean("apple"))
    )

    forAll(cases) { case (jsonPath, expected) =>
      val results = JsonBeanEvaluator.evaluate(jsonPath, root, Some(json))
      results.map(_.value) should contain theSameElementsAs expected
      results.foreach { case Node(location, value) =>
        JsonBeanEvaluator
          .singular(location.toAbsolutePath, json)
          .value
          .value should be(value)
      }
    }
  }

  it should "return atomic json values" in {
    val cases = Table(
      "json" -> "expected",
      JBoolean(true) -> True,
      JBoolean(false) -> False,
      JNull -> Null,
      JNumber(123) -> JNumber(123),
      JString("abc") -> JString("abc"),
      arr() -> arr(),
      obj() -> obj()
    )

    forAll(cases) { case (json, expected) =>
      val result = JsonBeanEvaluator
        .descend(Node(JsonPath.$, json))
        .loneElement
      result.value should be(expected)
      result.location should be(JsonPath.$)
    }
  }

  it should "not expose atomic values in arrays" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jarray = JArray(values)

    val result = JsonBeanEvaluator
      .descend(Node(JsonPath.$, jarray))
      .loneElement
    result.value should be(jarray)
    result.location should be(JsonPath.$)
  }

  it should "not expose atomic values in objects" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jobject = JObject(values.zipWithIndex.map { case (value, index) =>
      s"key$index" -> value
    }.toMap)

    val result = JsonBeanEvaluator
      .descend(Node(JsonPath.$, jobject))
      .loneElement
    result.value should be(jobject)
    result.location should be(JsonPath.$)
  }

  it should "recursively expand arrays and objects" in {
    val innerArray = arr(False, True)
    val innerObject = obj(
      "foo" -> JNumber(1),
      "inner" -> innerArray
    )
    val outerArray = arr(JString("foobar"), innerObject)

    val json = outerArray

    val results = JsonBeanEvaluator
      .descend(Node(JsonPath.$, json))
    results.map(_.value) should contain theSameElementsAs Seq(
      innerArray,
      innerObject,
      outerArray
    )
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, json).value.value should be(value)
    }
  }

  "attribute" should "select an object's attribute by right" in {
    val givenAttributeName = "testAttributeName"
    val givenAttributeValue = JsonBean.string("Test attribute right!")
    val givenJson = JsonBean.obj(
      givenAttributeName -> givenAttributeValue
    )
    val result = JsonBeanEvaluator
      .attribute(Node(JsonPath.$, givenJson), givenAttributeName)
      .value
    result.value should be(givenAttributeValue)
    result.location should be(JsonPath.$ / givenAttributeName)
  }

  it should "return none if the given attribute does not exist" in {
    val givenAttributeName = "testAttributeName"
    val givenJson = JsonBean.obj()

    JsonBeanEvaluator.attribute(
      Node(JsonPath.$, givenJson),
      givenAttributeName
    ) should be(empty)
  }

  it should "return none if the given json is not an object" in {
    val givenAttributeName = "testAttributeName"
    val cases = Table(
      "json",
      JsonBean.number(1),
      JsonBean.True,
      JsonBean.string("foobar"),
      JsonBean.arr()
    )

    forAll(cases) { givenJson =>
      JsonBeanEvaluator.attribute(
        Node(JsonPath.$, givenJson),
        givenAttributeName
      ) should be(empty)
    }
  }

  "index" should "select an array's right" in {
    val givenIndex = 0
    val givenIndexValue = JsonBean.string("Test attribute right!")
    val givenJson = JsonBean.arr(
      givenIndexValue
    )

    val result = JsonBeanEvaluator
      .index(Node(JsonPath.$, givenJson), givenIndex)
      .value
    result.value should be(givenIndexValue)
    result.location should be(JsonPath.$ / givenIndex)
  }

  it should "return none if the given right does not exist" in {
    val givenIndex = 0
    val givenJson = JsonBean.arr()

    JsonBeanEvaluator.index(Node(JsonPath.$, givenJson), givenIndex) should be(
      empty
    )
  }

  it should "return none if the given json is not an array" in {
    val givenIndex = 0
    val cases = Table(
      "json",
      JsonBean.number(1),
      JsonBean.True,
      JsonBean.string("foobar"),
      JsonBean.obj()
    )

    forAll(cases) { givenJson =>
      JsonBeanEvaluator.index(
        Node(JsonPath.$, givenJson),
        givenIndex
      ) should be(empty)
    }
  }

  "Wildcard" should "return nothing on atomic values" in {
    val cases = Table(
      "json",
      JsonBean.number(1),
      JsonBean.True,
      JsonBean.string("foobar")
    )

    forAll(cases) { json =>
      JsonBeanEvaluator.wildcard(Node(JsonPath.$, json)) should be(empty)
    }
  }

  it should "return the values of an array" in {
    val values = Vector(
      JsonBean.number(1),
      JsonBean.string("foobar"),
      JsonBean.False,
      JsonBean.obj("deadbeef" -> JsonBean.True),
      JsonBean.arr(JsonBean.True)
    )
    val json = JsonBean.fromValues(values)

    val results = JsonBeanEvaluator.wildcard(Node(JsonPath.$, json))
    results.map(_.value) should contain theSameElementsAs values
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, json).value.value should be(value)
    }
  }

  it should "return the attribute values of an object" in {
    val values = Vector(
      JsonBean.number(1),
      JsonBean.string("foobar"),
      JsonBean.False,
      JsonBean.obj("deadbeef" -> JsonBean.True),
      JsonBean.arr(JsonBean.True)
    )
    val json = JsonBean.fromAttributes(values.zipWithIndex.map {
      case (value, index) =>
        index.toString -> value
    })

    val results = JsonBeanEvaluator.wildcard(Node(JsonPath.$, json))
    results.map(_.value) should contain theSameElementsAs values
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, json).value.value should be(value)
    }
  }

  "Union" should "select indices from arrays" in {
    val givenJson = JsonBean.fromValues(
      Seq.tabulate(5)(JsonBean.number)
    )
    val targetIndices = Seq(0, 2, 4)
    val union = Union(targetIndices.head, targetIndices(1), targetIndices(2))

    val results = JsonBeanEvaluator
      .union(
        givenJson,
        Node(JsonPath.$, givenJson),
        union
      )
    results.map(_.value) should contain theSameElementsAs targetIndices.map(
      JsonBean.number
    )
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "select target attributes from objects" in {
    val raw = Map(
      "a" -> JsonBean.number(0),
      "b" -> JsonBean.number(1),
      "c" -> JsonBean.number(2),
      "d" -> JsonBean.number(3),
      "e" -> JsonBean.number(4)
    )
    val givenJson = JsonBean.JObject(raw)
    val targetAttributes = Seq("a", "c", "e")
    val union =
      Union(targetAttributes.head, targetAttributes(1), targetAttributes(2))

    val results = JsonBeanEvaluator.union(
      givenJson,
      Node(JsonPath.$, givenJson),
      union
    )
    results.map(_.value) should contain theSameElementsAs targetAttributes.map(
      raw
    )
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  "Slice" should "drop elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice.drop(2)
    )
    results.map(_.value) should contain theSameElementsAs elements.drop(2)
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "reflect the array with a start of 0" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice.start(0)
    )
    results.map(_.value) should contain theSameElementsAs elements
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "take elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice.take(2)
    )
    results.map(_.value) should contain theSameElementsAs elements.take(2)
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "take right with a negative start" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice.start(-2)
    )
    results.map(_.value) should contain theSameElementsAs elements.takeRight(2)
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "drop right with a negative end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice.end(-2)
    )
    results.map(_.value) should contain theSameElementsAs elements
      .dropRight(2)
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "take a sub array with start and end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val results = JsonBeanEvaluator.slice(
      Node(JsonPath.$, givenJson),
      Slice(2, 4)
    )
    results.map(_.value) should contain theSameElementsAs elements.slice(2, 4)
    results.foreach { case Node(location, value) =>
      JsonBeanEvaluator.singular(location, givenJson).value.value should be(
        value
      )
    }
  }

  it should "take every Nth element via step" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val cases = Table(
      "step" -> "expected elements",
      1 -> elements,
      2 -> Seq(0, 2, 4).map(elements),
      3 -> Seq(0, 3).map(elements),
      4 -> Seq(0, 4).map(elements),
      5 -> Seq(0).map(elements)
    )

    forAll(cases) { case (step, expected) =>
      val results = JsonBeanEvaluator.slice(
        Node(JsonPath.$, givenJson),
        Slice.everyN(step)
      )
      results.map(_.value) should contain theSameElementsAs expected
      results.foreach { case Node(location, value) =>
        JsonBeanEvaluator.singular(location, givenJson).value.value should be(
          value
        )
      }
    }
  }
}
