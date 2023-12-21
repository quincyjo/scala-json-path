package com.quincyjo.jsonpath

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import JsonBean.*
import com.quincyjo.jsonpath.JsonPath.RecursiveDescent
import org.scalatest.LoneElement
import org.scalatest.prop.TableDrivenPropertyChecks

class RecursiveDescentSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with TableDrivenPropertyChecks {

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
      RecursiveDescent(None).apply(json).loneElement should be(expected)
    }
  }

  it should "not expose atomic values in arrays" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jarray: JsonBean = JArray(values)

    RecursiveDescent(None).apply(jarray).loneElement should be(jarray)
  }

  it should "not expose atomic values in objects" in {
    val values = Vector(
      JNumber(1),
      JString("foobar"),
      False
    )
    val jobject: JsonBean = JObject(values.zipWithIndex.map {
      case (value, index) => s"key$index" -> value
    }.toMap)

    RecursiveDescent(None)(jobject).loneElement should be(jobject)
  }

  it should "recursively expand arrays and objects" in {
    val innerArray = arr(False, True)
    val innerObject = obj(
      "foo" -> JNumber(1),
      "inner" -> innerArray
    )
    val outerArray = arr(JString("foobar"), innerObject)

    val json: JsonBean = outerArray

    RecursiveDescent(None).apply(json) should contain theSameElementsAs
      Seq(innerArray, innerObject, outerArray)
  }
}
