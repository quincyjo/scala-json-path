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

  "areSameType" should "follow JS conversion rules" in {
    val cases = Table(
      ("left", "right", "expected"),
      (JsonBean.Null, JsonBean.Null, true),
      (JsonBean.True, JsonBean.False, true),
      (JsonBean.string("foobar"), JsonBean.string("deadbeef"), true),
      (JsonBean.number(42), JsonBean.number(42), true),
      (JsonBean.arr(), JsonBean.arr(), true),
      (JsonBean.obj(), JsonBean.obj(), true),
      (JsonBean.arr(), JsonBean.obj(), true),
      (JsonBean.Null, JsonBean.string("foobar"), false),
      (JsonBean.Null, JsonBean.number(42), false),
      (JsonBean.string(""), JsonBean.False, false)
    )

    forAll(cases) { (left, right, expected) =>
      jsonBeanSupport.areSameType(left, right) should be(expected)
    }
  }

  "convertTypes" should "follow JS comparison conversion" in {
    val cases = Table(
      ("a", "b", "expected"),
      (JsonBean.Null, JsonBean.Null, Some(JsonBean.Null, JsonBean.Null)),
      (JsonBean.True, JsonBean.False, Some(JsonBean.True, JsonBean.False)),
      (
        JsonBean.string("foobar"),
        JsonBean.string("deadbeef"),
        Some(JsonBean.string("foobar"), JsonBean.string("deadbeef"))
      ),
      (
        JsonBean.number(42),
        JsonBean.number(42),
        Some(JsonBean.number(42), JsonBean.number(42))
      ),
      (
        JsonBean.False,
        JsonBean.string("5"),
        Some(JsonBean.number(0), JsonBean.number(5))
      ),
      (
        JsonBean.True,
        JsonBean.string(""),
        Some(JsonBean.number(1), JsonBean.number(0))
      ),
      (
        JsonBean.arr(),
        JsonBean.False,
        Some(JsonBean.number(0), JsonBean.number(0))
      ),
      (JsonBean.obj(), JsonBean.True, None),
      (JsonBean.arr("foobar"), JsonBean.True, None),
      (JsonBean.Null, JsonBean.False, Some(JsonBean.Null, JsonBean.False))
    )

    forAll(cases) { case (a, b, expected) =>
      jsonBeanSupport.convertTypes(a, b) should be(expected)
    }
  }
}
