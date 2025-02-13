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

package com.quincyjo.jsonpath.parser.util

import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class StringEscapesSpec
    extends AnyFlatSpecLike
    with Matchers
    with EitherValues
    with TableDrivenPropertyChecks {

  "processEscapes" should "process basic escape sequences" in {
    val cases = Table(
      "input" -> "expected",
      "\\b" -> "\b",
      "\\t" -> "\t",
      "\\n" -> "\n",
      "\\f" -> "\f",
      "\\r" -> "\r",
      "\\\\" -> "\\",
      "\\/" -> "/",
      "\\'" -> "'",
      "\\\"" -> "\"",
      "\\u0061" -> "a"
    )

    forAll(cases) { case (input, expected) =>
      val result = StringEscapes.processEscapes(input).value
      result.value should be(expected)
      result.raw should be(input)
    }
  }

  it should "process strings iteratively" in {
    val cases = Table(
      "input" -> "expected",
      "foobar" -> "foobar",
      "\\\\t" -> "\\t",
      "across \\\"time\\\" is fun" -> "across \"time\" is fun"
    )

    forAll(cases) { case (input, expected) =>
      val result = StringEscapes.processEscapes(input).value
      result.value should be(expected)
      result.raw should be(input)
    }
  }

  it should "fail on invalid escape sequences" in {
    val cases = Table(
      "input",
      "\\",
      "\\x",
      "\\u",
      "\\u1",
      "\\u12",
      "\\u123"
    )

    forAll(cases) { input =>
      StringEscapes.processEscapes(input).isLeft should be(true)
    }
  }

  "takeQuotedString" should "take simple quoted strings" in {
    val cases = Table(
      ("input", "expected", "raw"),
      ("\"foobar\" and some extra", "foobar", "\"foobar\""),
      ("'foobar' and some extra", "foobar", "'foobar'")
    )

    forAll(cases) { case (input, expected, raw) =>
      val result = StringEscapes.takeQuotedString(input).value
      result.value should be(expected)
      result.raw should be(raw)
    }
  }

  it should "handle escaped quotes" in {
    val cases = Table(
      "input" -> "expected",
      "\"\\\"Proper Noun\\\"\"" -> "\"Proper Noun\""
    )

    forAll(cases) { case (input, expected) =>
      val result = StringEscapes.takeQuotedString(input).value
      result.value should be(expected)
      result.raw should be(input)
    }
  }

  it should "fail on unclosed quotes" in {
    val cases = Table(
      "input",
      "\"foobar",
      "'foobar"
    )

    forAll(cases) { input =>
      StringEscapes.takeQuotedString(input).isLeft should be(true)
    }
  }

  it should "return immediately if the first character is not a quote" in {
    val result = StringEscapes.takeQuotedString("foobar").value

    result.value should be("")
    result.index should be(0)
  }

  "escapeSingleQuotes" should "escape basic characters" in {
    val cases = Table(
      "charcter" -> "escape sequence",
      "\b" -> "\\b",
      "\t" -> "\\t",
      "\n" -> "\\n",
      "\f" -> "\\f",
      "\r" -> "\\r",
      "\\" -> "\\\\",
      "'" -> "\\'"
    )

    forAll(cases) { case (char, expected) =>
      StringEscapes.escapeSingleQuotes(char) should be(expected)
    }
  }
}
