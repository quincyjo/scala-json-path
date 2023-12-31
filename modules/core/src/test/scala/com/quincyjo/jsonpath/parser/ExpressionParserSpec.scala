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
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ExpressionParserSpec
    extends AnyFlatSpecLike
    with Matchers
    with ParseResultValues
    with TableDrivenPropertyChecks {

  "parse" should "handle basic expressions" in {
    val cases = Table(
      "input" -> "expected",
      "5 == 5" -> Equal(JsonNumber(5), JsonNumber(5)),
      "5 != 5" -> NotEqual(JsonNumber(5), JsonNumber(5)),
      "5 > 5" -> GreaterThan(JsonNumber(5), JsonNumber(5)),
      "5 >= 5" -> GreaterThanOrEqualTo(JsonNumber(5), JsonNumber(5)),
      "'a' < 'b'" -> LessThan(JsonString("a"), JsonString("b")),
      "5 <= 5" -> LessThanOrEqualTo(JsonNumber(5), JsonNumber(5)),
      "\"foobar\" + \"barfoo\"" -> Plus(
        JsonString("foobar"),
        JsonString("barfoo")
      ),
      "5 - 5" -> Minus(JsonNumber(5), JsonNumber(5)),
      "5 * 5" -> Multiply(JsonNumber(5), JsonNumber(5)),
      "5 / 5" -> Divide(JsonNumber(5), JsonNumber(5))
    )

    forAll(cases) { (input, expected) =>
      ExpressionParser.parse(input).value should be(expected)
    }
  }

  it should "operate from left to right" in {
    val cases = Table(
      "input" -> "expected",
      "5 == 5 == 5" -> Equal(
        Equal(JsonNumber(5), JsonNumber(5)),
        JsonNumber(5)
      ),
      "'abc' + 'def' == 'abcdef'" -> Equal(
        Plus(JsonString("abc"), JsonString("def")),
        JsonString("abcdef")
      )
    )

    forAll(cases) { (input, expected) =>
      ExpressionParser.parse(input).value should be(expected)
    }
  }

  it should "respect parenthesis" in {
    val cases = Table(
      "input" -> "expected",
      "(5 + 5)" -> Plus(JsonNumber(5), JsonNumber(5)),
      "(5 == 5) == 5" -> Equal(
        Equal(JsonNumber(5), JsonNumber(5)),
        JsonNumber(5)
      ),
      "5 == (5 == 5)" -> Equal(
        JsonNumber(5),
        Equal(JsonNumber(5), JsonNumber(5))
      ),
      "(5 == 5) == (5 == 5)" -> Equal(
        Equal(JsonNumber(5), JsonNumber(5)),
        Equal(JsonNumber(5), JsonNumber(5))
      ),
      "((5 == 5)) == 5" -> Equal(
        Equal(JsonNumber(5), JsonNumber(5)),
        JsonNumber(5)
      ),
      "((5 == 5) == 5)" -> Equal(
        Equal(JsonNumber(5), JsonNumber(5)),
        JsonNumber(5)
      )
    )

    forAll(cases) { (input, expected) =>
      ExpressionParser.parse(input).value should be(expected)
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
      ExpressionParser.parse(input).value should be(expected)
    }
  }

  it should "handle complex expressions" in {
    val cases = Table(
      "input" -> "expected",
      "@.author.lastName >= \"B\" && (@.cost <= $.maxCost)" ->
        And(
          GreaterThanOrEqualTo(
            JsonPathValue(JsonPath.`@` / "author" / "lastName"),
            JsonString("B")
          ),
          LessThanOrEqualTo(
            JsonPathValue(JsonPath.`@` / "cost"),
            JsonPathValue(JsonPath.$ / "maxCost")
          )
        )
    )

    forAll(cases) { (input, expected) =>
      ExpressionParser.parse(input).value should be(expected)
    }
  }
}
