package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.JsonBean._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonSupportSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  "coerceToNumber" should "follow JS conversion rules" in {
    val cases = Table(
      "json" -> "expected",
      JsonBean.number(42) -> Some(42),
      JsonBean.Null -> Some(0),
      JsonBean.True -> Some(1),
      JsonBean.False -> Some(0),
      JsonBean.string("42") -> Some(42),
      JsonBean.string("") -> Some(0),
      JsonBean.string("foobar") -> None,
      JsonBean.arr() -> Some(0),
      JsonBean.arr(JsonBean.number(1)) -> Some(1),
      JsonBean.fromValues(Vector.tabulate(5)(JsonBean.number)) -> None,
      JsonBean.obj() -> None,
      JsonBean.obj("foobar" -> 42) -> None
    )

    forAll(cases) { (json, expected) =>
      jsonBeanSupport.coerceToNumber(json) should be(expected)
    }
  }

  "coerceToString" should "follow JS conversion rules" in {
    val cases = Table(
      "json" -> "expected",
      JsonBean.Null -> "null",
      JsonBean.True -> "true",
      JsonBean.False -> "false",
      JsonBean.number(42) -> "42",
      JsonBean.string("foobar") -> "foobar",
      JsonBean.arr() -> "",
      JsonBean.arr("a", 1) -> "a,1",
      JsonBean.obj() -> "[object Object]",
      JsonBean.obj("foobar" -> 42) -> "[object Object]"
    )

    forAll(cases) { (json, expected) =>
      jsonBeanSupport.coerceToString(json) should be(expected)
    }
  }

  "coerceToBoolean" should "follow JS conversion rules" in {
    val cases = Table(
      "json" -> "expected",
      JsonBean.Null -> false,
      JsonBean.True -> true,
      JsonBean.False -> false,
      JsonBean.number(42) -> true,
      JsonBean.number(0) -> false,
      JsonBean.number(-1) -> true,
      JsonBean.string("foobar") -> true,
      JsonBean.string("") -> false,
      JsonBean.arr() -> true,
      JsonBean.arr("a", 1) -> true,
      JsonBean.obj() -> true,
      JsonBean.obj("foobar" -> 42) -> true
    )

    forAll(cases) { (json, expected) =>
      jsonBeanSupport.coerceToBoolean(json) should be(expected)
    }
  }
}
