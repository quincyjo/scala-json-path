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

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

trait JsonSupportSpecLike
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks
    with OptionValues {

  def supportFor[Json](implicit jsonSupport: JsonSupport[Json]): Unit = {

    def arr(json: Json*): Json = jsonSupport.fromValues(json)

    def fromValues(json: Iterable[Json]): Json = jsonSupport.fromValues(json)

    def obj(json: (String, Json)*): Json = jsonSupport.fromFields(json)

    def string(s: String): Json = jsonSupport.string(s)

    def number(n: Int): Json = jsonSupport.number(n)

    def boolean(b: Boolean): Json = jsonSupport.boolean(b)

    val Null: Json = jsonSupport.Null

    "asObject" should "be None for non-objects" in {
      val cases = Table(
        "json",
        arr(),
        fromValues(Vector.tabulate(5)(number)),
        boolean(true),
        boolean(false),
        string("foobar"),
        number(42),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.asObject(json) should be(empty)
      }
    }

    it should "return a map of an object's attributes" in {
      val cases = Table(
        "json" -> "expected",
        obj() -> Map.empty[String, Json],
        obj("foobar" -> number(42)) -> Map("foobar" -> number(42))
      )

      forAll(cases) { (json, expected) =>
        jsonSupport.asObject(json).value should be(expected)
      }
    }

    "asArray" should "be None for non-arrays" in {
      val cases = Table(
        "json",
        obj(),
        obj("foobar" -> number(42)),
        string("foobar"),
        number(42),
        boolean(true),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.asArray(json) should be(empty)
      }
    }

    it should "return the values of an array" in {
      val cases = Table(
        "json" -> "expected",
        arr() -> Vector.empty[Json],
        arr(number(1)) -> Vector(number(1)),
        arr(number(1), number(2), number(3)) -> Vector(
          number(1),
          number(2),
          number(3)
        )
      )

      forAll(cases) { (json, expected) =>
        jsonSupport
          .asArray(json)
          .value should contain theSameElementsAs expected
      }
    }

    "asString" should "be None for non-strings" in {
      val cases = Table(
        "json",
        number(42),
        boolean(true),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.asString(json) should be(empty)
      }
    }

    it should "be string value for JSON strings" in {
      val cases = Table(
        "json" -> "expected",
        string("foobar") -> "foobar",
        string("") -> "",
        string("apples and bananas") -> "apples and bananas"
      )

      forAll(cases) { (json, expected) =>
        jsonSupport.asString(json).value should be(expected)
      }
    }

    "asBoolean" should "be None for non-booleans" in {
      val cases = Table(
        "json",
        number(42),
        string("foobar"),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.asBoolean(json) should be(empty)
      }
    }

    it should "be boolean value for JSON booleans" in {
      val cases = Table(
        "json" -> "expected",
        boolean(true) -> true,
        boolean(false) -> false
      )

      forAll(cases) { (json, expected) =>
        jsonSupport.asBoolean(json).value should be(expected)
      }
    }

    "asNumber" should "be None for non-numbers" in {
      val cases = Table(
        "json",
        string("foobar"),
        boolean(true),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.asNumber(json) should be(empty)
      }
    }

    it should "be number value for JSON numbers" in {
      val cases = Table(
        "json" -> "expected",
        number(42) -> 42,
        number(0) -> 0
      )

      forAll(cases) { (json, expected) =>
        jsonSupport.asNumber(json).value should be(expected)
      }
    }

    "asNull" should "be None for non-nulls" in {
      val cases = Table(
        "json",
        string("foobar"),
        boolean(true),
        number(42),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42))
      )

      forAll(cases) { json =>
        jsonSupport.asNull(json) should be(empty)
      }
    }

    it should "be Unit for JSON nulls" in {
      jsonSupport.asNull(Null).value should be(())
    }

    "isObject" should "be None for non-objects" in {
      val cases = Table(
        "json",
        arr(),
        fromValues(Vector.tabulate(5)(number)),
        boolean(true),
        boolean(false),
        string("foobar"),
        number(42),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.isObject(json) should be(false)
      }
    }

    it should "return a map of an object's attributes" in {
      val cases = Table(
        "json",
        obj(),
        obj("foobar" -> number(42))
      )

      forAll(cases) { json =>
        jsonSupport.isObject(json) should be(true)
      }
    }

    "isArray" should "be None for non-arrays" in {
      val cases = Table(
        "json",
        obj(),
        obj("foobar" -> number(42)),
        string("foobar"),
        number(42),
        boolean(true),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.isArray(json) should be(false)
      }
    }

    it should "return the values of an array" in {
      val cases = Table(
        "json",
        arr(),
        arr(number(1)),
        arr(number(1), number(2), number(3))
      )

      forAll(cases) { json =>
        jsonSupport.isArray(json) should be(true)
      }
    }

    "isString" should "be None for non-strings" in {
      val cases = Table(
        "json",
        number(42),
        boolean(true),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.isString(json) should be(false)
      }
    }

    it should "be string value for JSON strings" in {
      val cases = Table(
        "json",
        string("foobar"),
        string(""),
        string("apples and bananas")
      )

      forAll(cases) { json =>
        jsonSupport.isString(json) should be(true)
      }
    }

    "isBoolean" should "be None for non-booleans" in {
      val cases = Table(
        "json",
        number(42),
        string("foobar"),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.isBoolean(json) should be(false)
      }
    }

    it should "be boolean value for JSON booleans" in {
      val cases = Table(
        "json",
        boolean(true),
        boolean(false)
      )

      forAll(cases) { json =>
        jsonSupport.isBoolean(json) should be(true)
      }
    }

    "isNumber" should "be None for non-numbers" in {
      val cases = Table(
        "json",
        string("foobar"),
        boolean(true),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42)),
        Null
      )

      forAll(cases) { json =>
        jsonSupport.isNumber(json) should be(false)
      }
    }

    it should "be number value for JSON numbers" in {
      val cases = Table(
        "json",
        number(42),
        number(0)
      )

      forAll(cases) { json =>
        jsonSupport.isNumber(json) should be(true)
      }
    }

    "isNull" should "be None for non-nulls" in {
      val cases = Table(
        "json",
        string("foobar"),
        boolean(true),
        number(42),
        arr(),
        arr(number(1), number(2), number(3)),
        obj(),
        obj("foobar" -> number(42))
      )

      forAll(cases) { json =>
        jsonSupport.isNull(json) should be(false)
      }
    }

    it should "be Unit for JSON nulls" in {
      jsonSupport.isNull(Null) should be(true)
    }
  }
}
