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

package com.quincyjo.jsonpath.parser

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.extensions.ArithmeticOperations._
import com.quincyjo.jsonpath.extensions._
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ExpressionParserSpec
    extends AnyFlatSpecLike
    with BeforeAndAfter
    with Matchers
    with ParseResultValues
    with TableDrivenPropertyChecks {

  private val expressionParser = ExpressionParser(
    extensions = List(
      Count.extension,
      Length.extension,
      Match.extension,
      Search.extension,
      Value.extension
    ),
    JsonPathParser.default,
    enableArithmeticOperators = true
  )

  "parse" should "handle basic expressions" in {
    val cases = Table(
      "input" -> "expected",
      "5 == 5" -> Equal(LiteralNumber(5), LiteralNumber(5)),
      "5 != 5" -> NotEqual(LiteralNumber(5), LiteralNumber(5)),
      "5 > 5" -> GreaterThan(LiteralNumber(5), LiteralNumber(5)),
      "5 >= 5" -> GreaterThanOrEqualTo(LiteralNumber(5), LiteralNumber(5)),
      "'a' < 'b'" -> LessThan(LiteralString("a"), LiteralString("b")),
      "5 <= 5" -> LessThanOrEqualTo(LiteralNumber(5), LiteralNumber(5)),
      "\"foobar\" + \"barfoo\"" -> Plus(
        LiteralString("foobar"),
        LiteralString("barfoo")
      ),
      "5 - 5" -> Minus(LiteralNumber(5), LiteralNumber(5)),
      "5 * 5" -> Multiply(LiteralNumber(5), LiteralNumber(5)),
      "5 / 5" -> Divide(LiteralNumber(5), LiteralNumber(5)),
      "true != false" -> NotEqual(LiteralBoolean(true), LiteralBoolean(false)),
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "fail on invalid expressions" in {
    val cases = Table(
      "input",
      "5 +",
      "+ 5",
      "5 - 1) * 2",
      "'foo''bar'",
      "length(1,2'foobar')",
      "length(1,2",
      "5 && true"
    )

    forAll(cases) { input =>
      expressionParser.parse(input).failed
    }
  }

  it should "operate from left to right" in {
    val cases = Table(
      "input" -> "expected",
      "'abc' + 'def' == 'abcdef'" -> Equal(
        Plus(LiteralString("abc"), LiteralString("def")),
        LiteralString("abcdef")
      )
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "respect parenthesis" in {
    val cases = Table(
      "input" -> "expected",
      "(5 + 5)" -> Plus(LiteralNumber(5), LiteralNumber(5)),
      "(5 * 5) == 5" -> Equal(
        Multiply(LiteralNumber(5), LiteralNumber(5)),
        LiteralNumber(5)
      ),
      "5 == (5 * 5)" -> Equal(
        LiteralNumber(5),
        Multiply(LiteralNumber(5), LiteralNumber(5))
      ),
      "(1 == 2) && (3 == 4)" -> And(
        Equal(LiteralNumber(1), LiteralNumber(2)),
        Equal(LiteralNumber(3), LiteralNumber(4))
      ),
      "((1 + 2)) == 3" -> Equal(
        Plus(LiteralNumber(1), LiteralNumber(2)),
        LiteralNumber(3)
      ),
      "((1 + 2) == 3)" -> Equal(
        Plus(LiteralNumber(1), LiteralNumber(2)),
        LiteralNumber(3)
      )
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "handle unary NOT (!) operator" in {
    val cases = Table(
      "input" -> "expected",
      "!(1 == 2)" -> Not(Equal(LiteralNumber(1), LiteralNumber(2))),
      "!@.foobar" -> Not(JsonPathValue(JsonPath.`@` / "foobar")),
      "!(1 == 2) && !(3 == 4)" -> And(
        Not(Equal(LiteralNumber(1), LiteralNumber(2))),
        Not(Equal(LiteralNumber(3), LiteralNumber(4)))
      ),
      "!(1 == 2) && !@.foobar" -> And(
        Not(Equal(LiteralNumber(1), LiteralNumber(2))),
        Not(JsonPathValue(JsonPath.`@` / "foobar"))
      )
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "parse JSON paths" in {
    val cases = Table(
      "input" -> "expected",
      "$.foo" -> JsonPathValue(JsonPath.$ / "foo"),
      "$.foo.bar" -> JsonPathValue(JsonPath.$ / "foo" / "bar"),
      "$.foo[0]" -> JsonPathValue(JsonPath.$ / "foo" / 0),
      "@['deadbeef']" -> JsonPathValue(JsonPath.`@` / "deadbeef")
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "handle complex expressions" in {
    val cases = Table(
      "input" -> "expected",
      "@.author.lastName >= \"B\" && (@.cost <= $.maxCost)" ->
        And(
          GreaterThanOrEqualTo(
            JsonPathValue(JsonPath.`@` / "author" / "lastName"),
            LiteralString("B")
          ),
          LessThanOrEqualTo(
            JsonPathValue(JsonPath.`@` / "cost"),
            JsonPathValue(JsonPath.$ / "maxCost")
          )
        )
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "bind conjunctions more tightly than disjunctions" in {
    val value = JsonPathValue(JsonPath.`@`)

    val cases = Table(
      "input" -> "expected",
      "@ || @ && @" -> Or(value, And(value, value)),
      "@ && @ || @" -> Or(And(value, value), value),
      "@ && @ || @ && @" -> Or(And(value, value), And(value, value)),
      "@ || @ && @ || @" -> Or(Or(value, And(value, value)), value)
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }

  it should "parse nested functions" in {
    val raw = s"length(value(@.foo)) > 3"

    expressionParser.parse(raw).value should be(
      GreaterThan(
        Length(Value(JsonPathValue(JsonPath.`@` / "foo"))),
        LiteralNumber(3)
      )
    )
  }

  it should "parse expressions as function arguments" in {
    val raw = s"""length(\"foo\" + \"bar\") > 3"""

    expressionParser.parse(raw).value should be(
      GreaterThan(
        Length(Plus(LiteralString("foo"), LiteralString("bar"))),
        LiteralNumber(3)
      )
    )
  }

  it should "fail if the parameters are the incorrect type" in {
    val cases = Table(
      "input",
      "length(@..*)",
    )

    forAll(cases) { input =>
      expressionParser.parse(input).failed
    }
  }

  it should "fail if a group is empty" in {
    val raw = s"""()"""

    val error = expressionParser.parse(raw).failed

    error.message.toLowerCase should include("empty parentheses")
  }

  it should "fail to parse arithmetic expressions if they are disabled" in {
    val newParser = expressionParser.copy(enableArithmeticOperators = false)

    val cases = Table(
      "input",
      "5 + 5",
      "5 - 5",
      "5 * 5",
      "5 / 5"
    )

    forAll(cases) { input =>
      newParser.parse(input).failed.message.toLowerCase should include(
        "arithmetic operators are disabled"
      )
    }
  }

  it should "respect arithmetic priority" in {
    val cases = Table(
      "input" -> "expected",
      "5 + 5 * 5" -> Plus(
        LiteralNumber(5),
        Multiply(LiteralNumber(5), LiteralNumber(5))
      ),
      "5 - 5 * 5" -> Minus(
        LiteralNumber(5),
        Multiply(LiteralNumber(5), LiteralNumber(5))
      ),
      "5 * 5 + 5" -> Plus(
        Multiply(LiteralNumber(5), LiteralNumber(5)),
        LiteralNumber(5)
      ),
      "5 * 5 - 5" -> Minus(
        Multiply(LiteralNumber(5), LiteralNumber(5)),
        LiteralNumber(5)
      ),
      "5 + 5 / 5" -> Plus(
        LiteralNumber(5),
        Divide(LiteralNumber(5), LiteralNumber(5))
      ),
      "5 - 5 / 5" -> Minus(
        LiteralNumber(5),
        Divide(LiteralNumber(5), LiteralNumber(5))
      ),
      "5 / 5 + 5" -> Plus(
        Divide(LiteralNumber(5), LiteralNumber(5)),
        LiteralNumber(5)
      ),
      "5 / 5 - 5" -> Minus(
        Divide(LiteralNumber(5), LiteralNumber(5)),
        LiteralNumber(5)
      )
    )

    forAll(cases) { (input, expected) =>
      expressionParser.parse(input).value should be(expected)
    }
  }
}
