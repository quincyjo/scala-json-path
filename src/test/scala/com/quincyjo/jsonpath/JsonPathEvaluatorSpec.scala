package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.JsonBean._
import com.quincyjo.jsonpath.JsonPath.{RecursiveDescent, Slice, Union, Wildcard}
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathEvaluatorSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with TableDrivenPropertyChecks {

  "evaluate" should "select according to the path" in {
    val firstName = "Jane"
    val lastName = "Doe"
    val streetAddress = "123 Sesame St"
    val city = "Cityville"
    val country = "ABC"
    val mobilePhoneNumber = "555-111-2222"
    val homePhoneNumber = "555-333-4444"
    val address = JsonBean.obj(
      "streetAddress" -> streetAddress,
      "city" -> city,
      "country" -> country
    )
    val mobilePhone = JsonBean.obj(
      "type" -> "mobile",
      "number" -> mobilePhoneNumber
    )
    val homePhone = JsonBean.obj(
      "type" -> "home",
      "number" -> homePhoneNumber
    )
    val phoneNumbers = JsonBean.arr(mobilePhone, homePhone)
    val json = JsonBean.obj(
      "firstName" -> "Jane",
      "lastName" -> "Doe",
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
      JsonPath.$ / RecursiveDescent() -> List(
        json,
        address,
        phoneNumbers,
        mobilePhone,
        homePhone
      ),
      JsonPath.$ / RecursiveDescent() / Wildcard -> List(
        JsonBean(firstName),
        JsonBean(lastName),
        address,
        phoneNumbers,
        JsonBean(streetAddress),
        JsonBean(city),
        JsonBean(country),
        mobilePhone,
        homePhone,
        JsonBean("mobile"),
        JsonBean(mobilePhoneNumber),
        JsonBean("home"),
        JsonBean(homePhoneNumber)
      )
    )

    forAll(cases) { case (jsonPath, expected) =>
      JsonBeanEvaluator.evaluate(
        jsonPath,
        json
      ) should contain theSameElementsAs expected
    }
  }

  it should "be against the current json if dynamic" in {
    val json = JsonBean.obj(
      "value" -> "fruits",
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
      JsonPath.`@` / "value" -> List(JsonBean("fruits")),
      JsonPath.`@` / "items" / 0 -> List(JsonBean("apple"))
    )

    forAll(cases) { case (jsonPath, expected) =>
      JsonBeanEvaluator.evaluate(
        jsonPath,
        root,
        Some(json)
      ) should contain theSameElementsAs expected
    }
  }

  it should "resolve to nothing if there is no root" in {
    val json = JsonBean.obj(
      "foobar" -> "deadbeef"
    )

    val jsonPath = JsonPath.empty / "foobar"
    JsonBeanEvaluator.evaluate(jsonPath, json) should be(empty)
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
      JsonBeanEvaluator.descend(json).loneElement should be(expected)
    }
  }

  it should "not expose atomic values in arrays" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jarray = JArray(values)

    JsonBeanEvaluator.descend(jarray).loneElement should be(jarray)
  }

  it should "not expose atomic values in objects" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jobject = JObject(values.zipWithIndex.map {
      case (value, index) => s"key$index" -> value
    }.toMap)

    JsonBeanEvaluator.descend(jobject).loneElement should be(jobject)
  }

  it should "recursively expand arrays and objects" in {
    val innerArray = arr(False, True)
    val innerObject = obj(
      "foo" -> JNumber(1),
      "inner" -> innerArray
    )
    val outerArray = arr(JString("foobar"), innerObject)

    val json = outerArray

    JsonBeanEvaluator.descend(json) should contain theSameElementsAs
      Seq(innerArray, innerObject, outerArray)
  }

  "attribute" should "select an object's attribute by value" in {
    val givenAttributeName = "testAttributeName"
    val givenAttributeValue = JsonBean.string("Test attribute value!")
    val givenJson = JsonBean.obj(
      givenAttributeName -> givenAttributeValue
    )
    JsonBeanEvaluator
      .attribute(givenJson, givenAttributeName)
      .loneElement should be(
      givenAttributeValue
    )
  }

  it should "return none if the given attribute does not exist" in {
    val givenAttributeName = "testAttributeName"
    val givenJson = JsonBean.obj()

    JsonBeanEvaluator.attribute(givenJson, givenAttributeName) should be(empty)
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
      JsonBeanEvaluator.attribute(givenJson, givenAttributeName) should be(
        empty
      )
    }
  }

  "index" should "select an array's value" in {
    val givenIndex = 0
    val givenIndexValue = JsonBean.string("Test attribute value!")
    val givenJson = JsonBean.arr(
      givenIndexValue
    )
    JsonBeanEvaluator.index(givenJson, givenIndex).loneElement should be(
      givenIndexValue
    )
  }

  it should "return none if the given value does not exist" in {
    val givenIndex = 0
    val givenJson = JsonBean.arr()

    JsonBeanEvaluator.index(givenJson, givenIndex) should be(empty)
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
      JsonBeanEvaluator.index(givenJson, givenIndex) should be(empty)
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
      JsonBeanEvaluator.wildcard(json) should be(empty)
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

    JsonBeanEvaluator.wildcard(json) should contain theSameElementsAs values
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

    JsonBeanEvaluator.wildcard(json) should contain theSameElementsAs values
  }

  "Union" should "select indices from arrays" in {
    val givenJson = JsonBean.fromValues(
      Seq.tabulate(5)(JsonBean.number)
    )
    val targetIndices = Seq(0, 2, 4)
    val union = Union(targetIndices.head, targetIndices(1), targetIndices(2))

    JsonBeanEvaluator.union(
      givenJson,
      union
    ) should contain theSameElementsAs targetIndices.map(
      JsonBean.number
    )
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

    JsonBeanEvaluator.union(
      givenJson,
      union
    ) should contain theSameElementsAs targetAttributes.map(raw)
  }

  "Slice" should "drop elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice.drop(2)
    ) should contain theSameElementsAs elements.drop(2)
  }

  it should "reflect the array with a start of 0" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice.start(0)
    ) should contain theSameElementsAs elements
  }

  it should "take elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice.take(2)
    ) should contain theSameElementsAs elements.take(2)
  }

  it should "take right with a negative start" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice.start(-2)
    ) should contain theSameElementsAs elements.takeRight(2)
  }

  it should "drop right with a negative end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice.end(-2)
    ) should contain theSameElementsAs elements
      .dropRight(2)
  }

  it should "take a sub array with start and end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    JsonBeanEvaluator.slice(
      givenJson,
      Slice(2, 4)
    ) should contain theSameElementsAs elements.slice(2, 4)
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
      JsonBeanEvaluator.slice(
        givenJson,
        Slice.everyN(step)
      ) should contain theSameElementsAs expected
    }
  }
}
