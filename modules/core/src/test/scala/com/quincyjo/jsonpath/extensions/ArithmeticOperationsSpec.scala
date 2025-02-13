package com.quincyjo.jsonpath.extensions

import com.quincyjo.braid.operations.implicits.toJsonOperationOps
import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.extensions.ArithmeticOperations._
import com.quincyjo.jsonpath.{JsonBean, JsonPath}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ArithmeticOperationsSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  private val evaluator = JsonBean.JsonBeanEvaluator

  "Plus" should behave like lowPrioritySerialization(Plus.apply)("+")

  it should behave like associativeSerialization(Plus.apply)("+")

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
      Plus(LiteralNumber(left), LiteralNumber(right))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ).value should be(
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
      Plus(LiteralNumber(number), LiteralNull)(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ).value should be(
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
      Plus(LiteralString(left), LiteralString(right))(
        evaluator,
        JsonBean.Null,
        JsonBean.Null
      ).value should be(JsonBean.string(left concat right))
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
      ).value should be(
        JsonBean.string(left.coerceToString concat right.coerceToString)
      )
    }
  }

  "Minus" should behave like lowPrioritySerialization(Minus.apply)("-")

  it should behave like arithmeticOperator(Minus.apply)(_ - _)

  it should behave like nonAssociativeSerialization(Minus.apply)("-")

  "Multiply" should behave like highPrioritySerialization(Multiply.apply)("*")

  it should behave like arithmeticOperator(Multiply.apply)(_ * _)

  it should behave like associativeSerialization(Multiply.apply)("*")

  "Divide" should behave like highPrioritySerialization(Divide.apply)("/")

  it should behave like arithmeticOperator(Divide.apply)(_ / _)

  it should behave like nonAssociativeSerialization(Divide.apply)("/")

  private def lowPrioritySerialization[T <: LowPriority](
      constructor: (ValueType, ValueType) => T
  )(symbol: String): Unit = {

    it should "serialize with the correct symbol" in {
      val left = LiteralNumber(42)
      val right = LiteralNumber(5)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }

    it should "not add redundant parenthesis to the left hand side" in {
      val left = Plus(LiteralNumber(5), LiteralNumber(5))
      val right = LiteralNumber(42)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }

    /*
    it should "add parenthesis to the right hand side" in {
      val left = LiteralNumber(42)
      val right = Plus(LiteralNumber(5), LiteralNumber(5))
      constructor(left, right).toString should be(
        s"$left $symbol ($right)"
      )
    }
     */
  }

  private def highPrioritySerialization[T <: HighPriority](
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

    it should "serialize the left hand with parentheses if necessary" in {
      val left = Plus(LiteralNumber(5), LiteralNumber(5))
      val right = LiteralNumber(42)
      constructor(left, right).toString should be(
        s"($left) $symbol $right"
      )
    }

    it should "not add redundant parenthesis to the left hand side" in {
      val left = Multiply(LiteralNumber(5), LiteralNumber(5))
      val right = LiteralNumber(42)
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }
  }

  def arithmeticOperator[T <: ArithmeticOperator](
      constructor: (ValueType, ValueType) => T
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
        constructor(LiteralNumber(left), LiteralNumber(right))(
          evaluator,
          JsonBean.Null,
          JsonBean.Null
        ).value should be(JsonBean.JNumber(f(left, right)))
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
        ).value should be(JsonBean.JNumber(f(coercedLeft, coercedRight)))
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
        ) should be(empty)
      }
    }
  }

  private def associativeSerialization[T <: ArithmeticOperator](
      constructor: (ValueType, ValueType) => T
  )(symbol: String): Unit = {

    it should "remove redundant parenthesis" in {
      val left = LiteralNumber(42)
      val right = constructor(LiteralNumber(5), LiteralNumber(5))
      constructor(left, right).toString should be(
        s"$left $symbol $right"
      )
    }
  }

  private def nonAssociativeSerialization[T <: ArithmeticOperator](
                                                                 constructor: (ValueType, ValueType) => T
                                                               )(symbol: String): Unit = {

    it should "group the right hand" in {
      val left = LiteralNumber(42)
      val right = constructor(LiteralNumber(5), LiteralNumber(5))
      constructor(left, right).toString should be(
        s"$left $symbol ($right)"
      )
    }
  }
}
