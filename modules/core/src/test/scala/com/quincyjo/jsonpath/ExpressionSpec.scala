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

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonSupport.Implicits.JsonSupportOps
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ExpressionSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  private val evaluator = JsonBean.JsonBeanEvaluator

  "JsonString" should "evaluate to a json string" in {
    JsonString("foo").apply(evaluator, JsonBean.Null, JsonBean.Null) should be(
      JsonBean.string("foo")
    )
  }

  it should "serialize to a JSON string" in {
    JsonString("foo").toString should be("\"foo\"")
  }

  it should "escape nested quotes" in {
    JsonString("foo\"bar").toString should be("\"foo\\\"bar\"")
  }

  "JsonNumber" should "evaluate to a json number" in {
    JsonNumber(42).apply(evaluator, JsonBean.Null, JsonBean.Null) should be(
      JsonBean.number(42)
    )
  }

  it should "serialize to a JSON number" in {
    JsonNumber(42).toString should be("42")
  }

  "JsonBoolean" should "evaluate to a json boolean" in {
    JsonBoolean(true).apply(
      evaluator,
      JsonBean.Null,
      JsonBean.Null
    ) should be(
      JsonBean.boolean(true)
    )
  }

  it should "serialize to a JSON boolean" in {
    JsonBoolean(true).toString should be("true")
    JsonBoolean(false).toString should be("false")
  }

  "Not" should behave like unarySerialization(Not.apply)("!")

  it should "be equivalent to JS falsey" in {
    val cases = Table(
      "json" -> "expected",
      JsonBean.boolean(true) -> false,
      JsonBean.boolean(false) -> true,
      JsonBean.number(0) -> true,
      JsonBean.number(42) -> false,
      JsonBean.number(-1) -> false,
      JsonBean.string("") -> true,
      JsonBean.string("foo") -> false,
      JsonBean.obj() -> false,
      JsonBean.obj("foo" -> "bar") -> false,
      JsonBean.arr() -> false,
      JsonBean.arr(5) -> false
    )

    forAll(cases) { case (json, expected) =>
      Not(JsonPathValue(JsonPath.$))(evaluator, json, json) should be(
        JsonBean.boolean(expected)
      )
    }
  }

  "Equal" should behave like binarySerialization(Equal.apply)("==")

  it should "compare same types" in {
    val cases = Table(
      ("left", "right", "expected"),
      (JsonNumber(42), JsonNumber(42), true),
      (JsonNumber(42), JsonNumber(5), false),
      (JsonNull, JsonNull, true),
      (JsonBoolean(true), JsonBoolean(true), true),
      (JsonBoolean(true), JsonBoolean(false), false),
      (JsonString("foobar"), JsonString("foobar"), true),
      (JsonString("foobar"), JsonString("deadbeef"), false)
    )

    forAll(cases) { case (left, right, expected) =>
      Equal(left, right)(evaluator, JsonBean.Null, JsonBean.Null) should be(
        JsonBean.boolean(expected)
      )
    }
  }

  it should "apply type conversion" in {
    val cases = Table(
      ("left", "right", "expected"),
      (JsonNumber(42), JsonNumber(5), false),
      (JsonNumber(42), JsonNumber(42), true),
      (JsonBoolean(true), JsonNumber(0), false),
      (JsonBoolean(false), JsonNumber(0), true),
      (JsonNull, JsonString(""), false),
      (JsonNull, JsonNull, true),
      (JsonString("5"), JsonNumber(5), true)
    )

    forAll(cases) { case (left, right, expected) =>
      Equal(left, right)(evaluator, JsonBean.Null, JsonBean.Null) should be(
        JsonBean.boolean(expected)
      )
    }
  }

  "GreaterThan" should behave like binarySerialization(GreaterThan.apply)(">")

  it should behave like comparator(GreaterThan.apply)(_ > _)

  "GreaterThanOrEqual" should behave like binarySerialization(
    GreaterThanOrEqualTo.apply
  )(
    ">="
  )

  it should behave like comparator(GreaterThanOrEqualTo.apply)(_ >= _)

  "LessThan" should behave like binarySerialization(LessThan.apply)("<")

  it should behave like comparator(LessThan.apply)(_ < _)

  "lessThanOrEqual" should behave like binarySerialization(
    LessThanOrEqualTo.apply
  )(
    "<="
  )

  it should behave like comparator(LessThanOrEqualTo.apply)(_ <= _)

  "And" should behave like binarySerialization(And.apply)("&&")

  "Or" should behave like binarySerialization(Or.apply)("||")

  "Plus" should behave like binarySerialization(Plus.apply)("+")

  it should "add two numbers" in {
    val cases = Table[BigDecimal, BigDecimal](
      ("left", "right"),
      (BigDecimal(42), BigDecimal(5)),
      (BigDecimal(42), BigDecimal(-5)),
      (BigDecimal(42), BigDecimal(0)),
      (BigDecimal(0), BigDecimal(42)),
      (BigDecimal(0), BigDecimal(0)),
      (BigDecimal(0), BigDecimal(-42))
    )

    forAll(cases) { case (left, right) =>
      Plus(JsonNumber(left), JsonNumber(right))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(
        JsonBean.JNumber(left + right)
      )
    }
  }

  it should "coerce null to 0" in {
    val cases = Table[BigDecimal](
      "number",
      BigDecimal(42),
      BigDecimal(0),
      BigDecimal(-42)
    )

    forAll(cases) { number =>
      Plus(JsonNumber(number), JsonNull)(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(
        JsonBean.JNumber(number)
      )
    }
  }

  it should "concat two strings" in {
    val cases = Table[String, String](
      ("left", "right"),
      ("foo", "bar"),
      ("", "bar"),
      ("foo", ""),
      ("", "")
    )

    forAll(cases) { case (left, right) =>
      Plus(JsonString(left), JsonString(right))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(JsonBean.string(left concat right))
    }
  }

  it should "coerce values to strings if both aren't numbers or null" in {
    val cases = Table[JsonBean, JsonBean](
      ("left", "right"),
      (JsonBean.number(42), JsonBean.string("foobar")),
      (JsonBean.string("foobar"), JsonBean.number(42)),
      (JsonBean.number(5), JsonBean.arr(JsonBean.True)),
      (JsonBean.arr(JsonBean.True), JsonBean.number(5)),
      (JsonBean.obj(), JsonBean.number(0))
    )

    forAll(cases) { case (left, right) =>
      Plus(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
        evaluator,
        left,
        right
      ) should be(
        JsonBean.string(left.coerceToString concat right.coerceToString)
      )
    }
  }

  "Minus" should behave like binarySerialization(Minus.apply)("-")

  it should behave like arithmeticOperator(Minus.apply)(_ - _)

  "Multiply" should behave like binarySerialization(Multiply.apply)("*")

  it should behave like arithmeticOperator(Multiply.apply)(_ * _)

  "Divide" should behave like binarySerialization(Divide.apply)("/")

  it should behave like arithmeticOperator(Divide.apply)(_ / _)

  def unarySerialization[T <: UnaryOperator](
      constructor: Expression => T
  )(symbol: String): Unit = {

    it should "serialize with the correct symbol" in {
      val expression = JsonNumber(42)
      constructor(expression).toString should be(s"$symbol$expression")
    }

    it should "serialize the expression with parentheses if necessary" in {
      val expression = Plus(JsonNumber(5), JsonNumber(5))
      constructor(expression).toString should be(s"$symbol($expression)")
    }
  }

  def binarySerialization[T <: BinaryOperator](
      constructor: (Expression, Expression) => T
  )(symbol: String): Unit = {

    it should "serialize with the correct symbol" in {
      val left = JsonNumber(42)
      val right = JsonNumber(5)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }

    it should "serialize the right hand with parentheses if necessary" in {
      val left = JsonNumber(42)
      val right = Plus(JsonNumber(5), JsonNumber(5))
      constructor(left, right).toString should be(
        s"$left $symbol ($right)"
      )
    }

    it should "not add redundant parenthesis to the left hand side" in {
      val left = Plus(JsonNumber(5), JsonNumber(5))
      val right = JsonNumber(42)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }
  }

  def comparator[T <: Comparator](
      constructor: (Expression, Expression) => T
  )(f: (Int, Int) => Boolean): Unit = {

    it should "compare numbers" in {
      val cases = Table(
        ("left", "right"),
        (42, 5),
        (5, 42),
        (42, 42)
      )

      forAll(cases) { case (a, b) =>
        constructor(JsonNumber(a), JsonNumber(b))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(JsonBean.boolean(f(a.compareTo(b), 0)))
      }
    }

    it should "compare strings alphabetically" in {
      val cases = Table(
        ("left", "right"),
        ("a", "b"),
        ("b", "a"),
        ("a", "a"),
        ("A", "a")
      )

      forAll(cases) { case (a, b) =>
        constructor(JsonString(a), JsonString(b))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(JsonBean.boolean(f(a.compareTo(b), 0)))
      }
    }

    it should "compare arrays by length" in {
      val cases = Table(
        ("left", "right"),
        (JsonBean.arr(), JsonBean.arr()),
        (JsonBean.arr(JsonBean.number(1)), JsonBean.arr()),
        (JsonBean.arr(), JsonBean.arr(JsonBean.number(1)))
      )

      forAll(cases) { case (left, right) =>
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(
          JsonBean.boolean(f(left.values.size.compareTo(right.values.size), 0))
        )
      }
    }

    it should "be false if either right is NaN" in {
      val cases = Table[JsonBean, JsonBean](
        ("left", "right"),
        (JsonBean.arr("foobar"), JsonBean.number(0)),
        (JsonBean.arr(1, 2, 3), JsonBean.number(0)),
        (JsonBean.number(1), JsonBean.obj()),
        (JsonBean.number(42), JsonBean.string("foobar"))
      )

      forAll(cases) { case (left, right) =>
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(JsonBean.boolean(false))
      }
    }
  }

  def arithmeticOperator[T <: ArithmeticOperator](
      constructor: (Expression, Expression) => T
  )(f: (BigDecimal, BigDecimal) => BigDecimal): Unit = {

    it should "operate on two numbers" in {
      val cases = Table[BigDecimal, BigDecimal](
        ("left", "right"),
        (1, 2),
        (-1, 2),
        (2, -1),
        (1, 1),
        (-1, -1),
        (1.1, 2.2),
        (4.4, 3.3)
      )

      forAll(cases) { case (left, right) =>
        constructor(JsonNumber(left), JsonNumber(right))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(JsonBean.JNumber(f(left, right)))
      }
    }

    it should "coerce values into numbers" in {
      val cases = Table[JsonBean, JsonBean](
        ("left", "right"),
        (JsonBean.Null, JsonBean.number(1)),
        (JsonBean.boolean(true), JsonBean.number(1)),
        (JsonBean.boolean(false), JsonBean.number(1)),
        (JsonBean.string(""), JsonBean.number(1)),
        (JsonBean.string("1"), JsonBean.number(1)),
        (JsonBean.arr(), JsonBean.number(1)),
        (JsonBean.arr(JsonBean.number(42)), JsonBean.number(1))
      )

      forAll(cases) { case (left, right) =>
        val coercedLeft = left.coerceToNumber.value
        val coercedRight = right.coerceToNumber.value
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(JsonBean.JNumber(f(coercedLeft, coercedRight)))
      }
    }

    it should "be null for NaN operands" in {
      val cases = Table(
        ("left", "right"),
        (JsonBean.string("foobar"), JsonBean.number(0)),
        (
          JsonBean.JArray(Vector.tabulate(3)((JsonBean.JNumber(_)))),
          JsonBean.number(0)
        ),
        (JsonBean.arr(JsonBean.string("foobar")), JsonBean.number(0)),
        (JsonBean.obj(), JsonBean.number(0))
      )

      forAll(cases) { case (left, right) =>
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(JsonBean.Null)
      }
    }
  }
}
