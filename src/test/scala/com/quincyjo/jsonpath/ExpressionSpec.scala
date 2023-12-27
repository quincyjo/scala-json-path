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

  "Not" should "be equivalent to JS falsey" in {
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

  it should "serialize as a ! unary operator" in {
    Not(JsonNumber(42)).toString should be("!42")
  }

  "OpenParenthesis" should "serialize as a as (<expression>)" in {
    Parenthesis(JsonNumber(42)).toString should be("(42)")
  }

  "GreaterThan" should "serialize as a > operator" in {
    GreaterThan(JsonNumber(42), JsonNumber(5)).toString should be(
      "42 > 5"
    )
  }

  it should behave like comparator(GreaterThan.apply)(_ > _)

  "GreaterThanOrEqual" should "serialize as a >= operator" in {
    GreaterThanOrEqualTo(JsonNumber(42), JsonNumber(5)).toString should be(
      "42 >= 5"
    )
  }

  it should behave like comparator(GreaterThanOrEqualTo.apply)(_ >= _)

  "LessThan" should "serialize as a < operator" in {
    LessThan(JsonNumber(42), JsonNumber(5)).toString should be(
      "42 < 5"
    )
  }

  it should behave like comparator(LessThan.apply)(_ < _)

  "LessThanOrEqual" should "serialize as a <= operator" in {
    LessThanOrEqualTo(JsonNumber(42), JsonNumber(5)).toString should be(
      "42 <= 5"
    )
  }

  it should behave like comparator(LessThanOrEqualTo.apply)(_ <= _)

  "And" should "serialize as a && operator" in {
    val a = GreaterThan(JsonNumber(42), JsonNumber(5))
    val b = LessThan(JsonNumber(42), JsonNumber(5))
    And(a, b).toString should be(s"$a && $b")
  }

  "Or" should "serialize as a || operator" in {
    val a = GreaterThan(JsonNumber(42), JsonNumber(5))
    val b = LessThan(JsonNumber(42), JsonNumber(5))
    Or(a, b).toString should be(s"$a || $b")
  }

  "Plus" should "serialize as a + operator" in {
    Plus(JsonNumber(42), JsonNumber(5)).toString should be("42 + 5")
  }

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

  "Minus" should "serialize as a - operator" in {
    Minus(JsonNumber(42), JsonNumber(5)).toString should be("42 - 5")
  }

  it should behave like arithmeticOperator(Minus.apply)(_ - _)

  "Multiply" should "serialize as a * operator" in {
    Multiply(JsonNumber(42), JsonNumber(5)).toString should be("42 * 5")
  }

  it should behave like arithmeticOperator(Multiply.apply)(_ * _)

  "Divide" should "serialize as a / operator" in {
    Divide(JsonNumber(42), JsonNumber(5)).toString should be("42 / 5")
  }

  it should behave like arithmeticOperator(Divide.apply)(_ / _)

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