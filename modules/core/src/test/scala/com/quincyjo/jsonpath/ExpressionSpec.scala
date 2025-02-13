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
import com.quincyjo.jsonpath.JsonBean.jsonBeanBraid
import com.quincyjo.jsonpath.extensions.ArithmeticOperations._
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

  "LiteralString" should "evaluate to a json string" in {
    LiteralString("foo")
      .apply(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      )
      .value should be(
      JsonBean.string("foo")
    )
  }

  it should "serialize to a JSON string" in {
    LiteralString("foo").toString should be("\"foo\"")
  }

  it should "escape nested quotes" in {
    LiteralString("foo\"bar").toString should be("\"foo\\\"bar\"")
  }

  "LiteralNumber" should "evaluate to a json number" in {
    LiteralNumber(42)
      .apply(evaluator, JsonBean.Null, JsonBean.Null)
      .value should be(
      JsonBean.number(42)
    )
  }

  it should "serialize to a JSON number" in {
    LiteralNumber(42).toString should be("42")
  }

  "LiteralBoolean" should "evaluate to a json boolean" in {
    LiteralBoolean(true)
      .apply(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      )
      .value should be(
      JsonBean.boolean(true)
    )
  }

  it should "serialize to a JSON boolean" in {
    LiteralBoolean(true).toString should be("true")
    LiteralBoolean(false).toString should be("false")
  }

  "LiteralNull" should "evaluate to JSON null" in {
    LiteralNull
      .apply(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      )
      .value should be(JsonBean.Null)
  }

  it should "serialize to JSON null" in {
    LiteralNull.toString should be("null")
  }

  "Not" should behave like unarySerialization(Not.apply)("!")

  it should "invert a logical types" in {
    val cases = Table(
      "json" -> "expected",
      JsonBean.obj("foobar" -> 42) -> false,
      JsonBean.obj("baz" -> 42) -> true
    )

    forAll(cases) { case (json, expected) =>
      Not(JsonPathValue(JsonPath.$ / "foobar"))(
        evaluator,
        json,
        json
      ) should be(
        expected
      )
    }
  }

  "Equal" should behave like binarySerialization(Equal.apply)("==")

  it should behave like nonComparableEquality(Equal.apply)

  "NotEqual" should behave like binarySerialization(NotEqual.apply)("!=")

  it should behave like nonComparableEquality(
    NotEqual.apply,
    equality => !equality
  )

  "GreaterThan" should behave like binarySerialization(GreaterThan.apply)(">")

  it should behave like comparator(GreaterThan.apply)(_ > _)

  "GreaterThanOrEqual" should behave like binarySerialization(
    GreaterThanOrEqualTo.apply
  )(
    ">="
  )

  it should behave like comparator(GreaterThanOrEqualTo.apply)(_ >= _)

  it should behave like equalityOperator(GreaterThanOrEqualTo.apply)

  "LessThan" should behave like binarySerialization(LessThan.apply)("<")

  it should behave like comparator(LessThan.apply)(_ < _)

  "lessThanOrEqual" should behave like binarySerialization(
    LessThanOrEqualTo.apply
  )(
    "<="
  )

  it should behave like comparator(LessThanOrEqualTo.apply)(_ <= _)

  it should behave like equalityOperator(LessThanOrEqualTo.apply)

  "And" should "serialize with the correct symbol" in {
    val expression = JsonPathValue(JsonPath.$)
    And(expression, expression).toString should be(
      s"$expression && $expression"
    )
  }

  it should "serialize according to binding priority" in {
    val value = JsonPathValue(JsonPath.`@`)
    val cases = Table(
      "expression" -> "expected",
      And(value, Or(value, value)) -> "@ && (@ || @)",
      And(Or(value, value), value) -> "(@ || @) && @",
      And(And(value, value), value) -> "@ && @ && @",
      And(value, And(value, value)) -> "@ && @ && @"
    )

    forAll(cases) { (expression, expected) =>
      expression.toString should be(expected)
    }
  }

  it should "be true only if both sides are true" in {
    val value = JsonBean.obj(
      "number" -> 42,
      "string" -> "foobar",
      "boolean" -> true
    )

    val cases = Table[LogicalType, LogicalType, Boolean](
      ("left", "right", "expected result"),
      (
        JsonPathValue(JsonPath.relative),
        JsonPathValue(JsonPath.relative),
        true
      ),
      (
        JsonPathValue(JsonPath.relative),
        JsonPathValue(JsonPath.relative / "doesNotExist"),
        false
      ),
      (
        JsonPathValue(JsonPath.relative / "doesNotExist"),
        JsonPathValue(JsonPath.relative),
        false
      ),
      (
        Equal(JsonPathValue(JsonPath.relative / "number"), LiteralNumber(42)),
        Equal(JsonPathValue(JsonPath.relative / "string"), LiteralNumber(42)),
        false
      ),
      (
        Equal(
          JsonPathValue(JsonPath.relative / "string"),
          LiteralString("foobar")
        ),
        GreaterThan(
          JsonPathValue(JsonPath.relative / "number"),
          LiteralNumber(10)
        ),
        true
      )
    )

    forAll(cases) { case (left, right, expected) =>
      And(left, right)(evaluator, value, value) should be(
        expected
      )
    }
  }

  "Or" should "serialize with the correct symbol" in {
    val expression = JsonPathValue(JsonPath.$)
    Or(expression, expression).toString should be(
      s"$expression || $expression"
    )
  }

  it should "serialize according to binding priority" in {
    val value = JsonPathValue(JsonPath.`@`)
    val cases = Table(
      "expression" -> "expected",
      Or(value, And(value, value)) -> "@ || @ && @",
      Or(And(value, value), value) -> "@ && @ || @",
      Or(Or(value, value), value) -> "@ || @ || @",
      Or(value, Or(value, value)) -> "@ || @ || @",
      Or(And(value, value), And(value, value)) -> "@ && @ || @ && @",
      Or(Or(value, And(value, value)), value) -> "@ || @ && @ || @"
    )

    forAll(cases) { (expression, expected) =>
      expression.toString should be(expected)
    }
  }

  it should "be true if both either side is true" in {
    val value = JsonBean.obj(
      "number" -> 42,
      "string" -> "foobar",
      "boolean" -> true
    )

    val cases = Table[LogicalType, LogicalType, Boolean](
      ("left", "right", "expected result"),
      (
        JsonPathValue(JsonPath.relative),
        JsonPathValue(JsonPath.relative),
        true
      ),
      (
        JsonPathValue(JsonPath.relative),
        JsonPathValue(JsonPath.relative / "doesNotExist"),
        true
      ),
      (
        JsonPathValue(JsonPath.relative / "doesNotExist"),
        JsonPathValue(JsonPath.relative),
        true
      ),
      (
        Equal(JsonPathValue(JsonPath.relative / "number"), LiteralNumber(42)),
        Equal(JsonPathValue(JsonPath.relative / "string"), LiteralNumber(42)),
        true
      ),
      (
        Equal(
          JsonPathValue(JsonPath.relative / "number"),
          LiteralString("foobar")
        ),
        Equal(JsonPathValue(JsonPath.relative / "string"), LiteralNumber(42)),
        false
      ),
      (
        Equal(
          JsonPathValue(JsonPath.relative / "string"),
          LiteralString("foobar")
        ),
        GreaterThan(
          JsonPathValue(JsonPath.relative / "number"),
          LiteralNumber(10)
        ),
        true
      )
    )

    forAll(cases) { case (left, right, expected) =>
      Or(left, right)(evaluator, value, value) should be(
        expected
      )
    }
  }

  private def unarySerialization[T <: UnaryOperator[?]](
      constructor: LogicalType => T
  )(symbol: String): Unit = {

    it should "serialize with the correct symbol" in {
      val expression = JsonPathValue(JsonPath.$)
      constructor(expression).toString should be(s"$symbol$expression")
    }

    it should "serialize the expression with parentheses if necessary" in {
      val expression = LessThanOrEqualTo(LiteralNumber(5), LiteralNumber(5))
      constructor(expression).toString should be(s"$symbol($expression)")
    }
  }

  private def binarySerialization[T <: BinaryOperator[ValueType, ValueType]](
      constructor: (ValueType, ValueType) => T
  )(symbol: String): Unit = {

    it should "serialize with the correct symbol" in {
      val left = LiteralNumber(42)
      val right = LiteralNumber(5)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }

    it should "serialize the right hand with parentheses if necessary" in {
      val left = LiteralNumber(42)
      val right = Plus(LiteralNumber(5), LiteralNumber(5))
      constructor(left, right).toString should be(
        s"$left $symbol ($right)"
      )
    }

    it should "not add redundant parenthesis to the left hand side" in {
      val left = Plus(LiteralNumber(5), LiteralNumber(5))
      val right = LiteralNumber(42)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }
  }

  def comparator[T <: Comparator](
      constructor: (ValueType, ValueType) => T
  )(f: (Int, Int) => Boolean): Unit = {

    it should "compare numbers" in {
      val cases = Table(
        ("left", "right"),
        (42, 5),
        (5, 42),
        (42, 42)
      )

      forAll(cases) { case (a, b) =>
        constructor(LiteralNumber(a), LiteralNumber(b))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(f(a.compareTo(b), 0))
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
        constructor(LiteralString(a), LiteralString(b))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(f(a.compareTo(b), 0))
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
        ) should be(false)
      }
    }
  }

  def nonComparableEquality[T <: IncludesEqualityCheck](
      constructor: (ValueType, ValueType) => T,
      mapEquality: Boolean => Boolean = identity
  ): Unit = {
    it should behave like equalityOperator(constructor, mapEquality)

    it should "compare atomics by value" in {
      val cases = Table(
        ("left", "right", "are equal"),
        (LiteralNumber(42), LiteralNumber(42), true),
        (LiteralNumber(42), LiteralNumber(5), false),
        (Expression.Null, Expression.Null, true),
        (Expression.True, Expression.True, true),
        (Expression.True, Expression.False, false),
        (LiteralString("foobar"), LiteralString("foobar"), true),
        (LiteralString("foobar"), LiteralString("deadbeef"), false)
      )

      forAll(cases) { case (left, right, areEqual) =>
        constructor(left, right)(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(
          mapEquality(areEqual)
        )
      }
    }

  }

  def equalityOperator[T <: IncludesEqualityCheck](
      constructor: (ValueType, ValueType) => T,
      mapEquality: Boolean => Boolean = identity
  ): Unit = {

    it should "be equal for identical atomics" in {
      val cases = Table(
        ("left", "right"),
        (LiteralNumber(42), LiteralNumber(42)),
        (LiteralBoolean(true), LiteralBoolean(true)),
        (LiteralString("foobar"), LiteralString("foobar"))
      )

      forAll(cases) { case (left, right) =>
        constructor(left, right)(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ) should be(mapEquality(true))
      }
    }

    it should "compare associatives by values" in {
      val cases = Table(
        ("left", "right", "expected"),
        (JsonBean.arr(), JsonBean.arr(), true),
        (JsonBean.arr(JsonBean.number(42)), JsonBean.arr(), false),
        (JsonBean.arr(), JsonBean.arr(JsonBean.number(42)), false),
        (
          JsonBean.arr(JsonBean.number(42)),
          JsonBean.arr(JsonBean.number(42)),
          true
        ),
        (JsonBean.obj("foo" -> "bar"), JsonBean.obj("foo" -> "bar"), true),
        (JsonBean.obj(), JsonBean.obj(), true),
        (JsonBean.obj("foo" -> "bar"), JsonBean.obj(), false),
        (
          JsonBean.obj("a" -> "this value"),
          JsonBean.obj("a" -> "that value"),
          false
        ),
        (
          JsonBean.obj("a" -> "foo"),
          JsonBean.obj("a" -> "foo", "b" -> "bar"),
          false
        )
      )

      forAll(cases) { case (left, right, expected) =>
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(mapEquality(expected))
      }
    }

    it should "be false if they are different types" in {
      val cases = Table[JsonBean, JsonBean](
        ("left", "right"),
        (JsonBean.number(42), JsonBean.string("foobar")),
        (JsonBean.string("foobar"), JsonBean.number(42)),
        (JsonBean.number(5), JsonBean.arr(JsonBean.True)),
        (JsonBean.arr(JsonBean.True), JsonBean.number(5)),
        (JsonBean.obj(), JsonBean.number(0))
      )

      forAll(cases) { case (left, right) =>
        constructor(JsonPathValue(JsonPath.$), JsonPathValue(JsonPath.`@`))(
          evaluator,
          left,
          right
        ) should be(mapEquality(false))
      }
    }

    it should "be true if both operands are Nothing" in {
      val nothingQuery = JsonPath.$ / "foobar"
      constructor(JsonPathValue(nothingQuery), JsonPathValue(nothingQuery))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(mapEquality(true))
    }

    it should "be false if only one operand is Nothing" in {
      val nothingQuery = JsonPath.$ / "foobar"
      constructor(JsonPathValue(JsonPath.$), JsonPathValue(nothingQuery))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(mapEquality(false))
    }
  }

}
