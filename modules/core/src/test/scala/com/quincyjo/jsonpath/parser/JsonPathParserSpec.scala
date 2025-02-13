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
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.extensions.{Count, Length, Match, Search, Value}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathParserSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks
    with ParseResultValues {

  "parse" should "parse all basic path nodes" in {
    val cases = Table(
      "input" -> "expected",
      "$" -> $,
      "$.foo" -> $ / "foo",
      "$[0]" -> $ / 0,
      "$.0" -> $ / 0,
      "$[0].foo" -> $ / 0 / "foo",
      "$[foo,bar]" -> $ / Union("foo", "bar"),
      "$[1,2]" -> $ / Union(1, 2),
      "$[foo,2]" -> $ / Union("foo", 2),
      "$[1,bar]" -> $ / Union(1, "bar"),
      "$[:1]" -> $ / Slice.take(1),
      "$[1:]" -> $ / Slice.drop(1),
      "$[::3]" -> $ / Slice.everyN(3),
      "$[1:2]" -> $ / Slice(1, 2),
      "$[1:2:3]" -> $ / Slice(1, 2, 3),
      "$.*.deadbeef" -> $ / Wildcard / "deadbeef",
      "$.store.book[*].author" -> $ / "store" / "book" / Wildcard / "author",
      "$..author" -> $ */ Attribute("author"),
      "$..['author']" -> $ */ Attribute("author"),
      "$.store.*" -> $ / "store" / Wildcard,
      "$.store..price" -> $ / "store" */ Attribute("price"),
      "$..book[2]" -> $ */ Attribute("book") / 2,
      "$..book[-1:]" -> $ */ Attribute("book") / Slice.takeRight(1),
      "$..book[0,1]" -> $ */ Attribute("book") / Union(0, 1),
      "$..book[:2]" -> $ */ Attribute("book") / Slice.take(2),
      "$..*" -> $ */ Wildcard
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "fail if it does not start with a valid root" in {
    val failure = JsonPathParser.default.parse("['foo']['bar]").failed

    failure.message should (include("$") and include("@"))
    failure.index should be(0)
  }

  it should "fail on invalid paths" in {
    val cases = Table(
      "input",
      "$..'foobar'",
      "$.'foobar'",
      "$$foobar",
      "$'foobar'",
      "$['foobar'",
      "$[?'foobar']",
      "$[1,2,'foo''bar']",
      "$[:]",
      "$[1:2:3:4]"
    )

    forAll(cases) { input =>
      JsonPathParser.default.parse(input).failed
    }
  }

  it should "handle quoted strings" in {
    val cases = Table(
      "input" -> "expected",
      "$['foo']" -> $ / "foo",
      "$[\"foo\"]" -> $ / "foo",
      "$[\"foo\",\"bar\",deadbeef]" -> $ / Union("foo", "bar", "deadbeef"),
      "$['foo','bar',deadbeef]" -> $ / Union("foo", "bar", "deadbeef"),
      "$['foo',\"bar\",deadbeef]" -> $ / Union("foo", "bar", "deadbeef")
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "handle quoted strings with escaped nested quotes" in {
    val cases = Table(
      "input" -> "expected",
      "$['ain\\'t that neat']" -> $ / "ain't that neat",
      "$[\"\\\"Proper Noun\\\"\"]" -> $ / "\"Proper Noun\""
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "parse expressions according to the configured expression parser" in {
    val cases = Table(
      "input" -> "expected",
      "$[?(@.foobar>3)]" -> $ /?
        GreaterThan(JsonPathValue(`@` / "foobar"), LiteralNumber(3)),
      "$[?(@.length >= 5 && @[5].isValid)]" -> $ / Filter(
        And(
          GreaterThanOrEqualTo(
            JsonPathValue(`@` / "length"),
            LiteralNumber(5)
          ),
          JsonPathValue(`@` / 5 / "isValid")
        )
      )
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "handle filter expressions without parentheses" in {
    val cases = Table(
      "input" -> "expected",
      "$[?@.foobar>3]" -> $ /? GreaterThan(
        JsonPathValue(`@` / "foobar"),
        LiteralNumber(3)
      ),
      "$[?@.foobar>3, 'foobar']" -> $ / Union(
        Filter(
          GreaterThan(JsonPathValue(`@` / "foobar"), LiteralNumber(3))
        ),
        Attribute("foobar")
      ),
      "@[5].isValid" -> `@` / 5 / "isValid",
      "$[?@.length >= 5 && @[5].isValid]" -> $ / Filter(
        And(
          GreaterThanOrEqualTo(
            JsonPathValue(`@` / "length"),
            LiteralNumber(5)
          ),
          JsonPathValue(`@` / 5 / "isValid")
        )
      )
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "parse complex unions" in {
    val cases = Table(
      "input" -> "expected",
      "$[1,'foo',?(@.keep),1:2:3,*]" -> $ / Union(
        Index(1),
        Attribute("foo"),
        Seq(Filter(JsonPathValue(`@` / "keep")), Slice(1, 2, 3), Wildcard)
      )
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  "take" should "read up to the first parse error" in {
    val cases = Table(
      ("input", "JsonPath", "raw"),
      ("$.foobar", $ / "foobar", "$.foobar"),
      ("$.foobar > 5", $ / "foobar", "$.foobar "),
      ("$ > 5", $, "$ "),
      ("@[:-1]", `@` / Slice.dropRight(1), "@[:-1]"),
      ("$['foobar']", JsonPath.$ / "foobar", "$['foobar']")
    )

    forAll(cases) { (input, jsonPath, raw) =>
      val result = JsonPathParser.default.take(input).value
      result.value should be(jsonPath)
      result.raw should be(raw)
    }
  }

  it should "decode escape control characters" in {
    val cases = Table(
      "input" -> "expected",
      "\\b" -> "\b",
      "\\t" -> "\t",
      "\\n" -> "\n",
      "\\f" -> "\f",
      "\\r" -> "\r",
      "\\\\" -> "\\",
      "\\\\\\'" -> "\\'",
      "\\/" -> "/",
      "\\'" -> "'",
      "\\\"" -> "\"",
      "\\u0061" -> "a"
    )

    forAll(cases) { case (input, expected) =>
      JsonPathParser.default.parse(s"$$['$input']").value should be(
        $ / expected
      )
    }
  }

  it should "parse extensions" in {
    val cases = Table(
      "input" -> "expected",
      s"$$[?(length(@.foo) > 3)]" -> $ /? GreaterThan(
        Length(JsonPathValue(`@` / "foo")),
        LiteralNumber(3)
      ),
      s"$$[?value(@) > 3]" -> $ /? GreaterThan(
        Value(JsonPathValue(`@`)),
        LiteralNumber(3)
      ),
      s"$$[?count(@['items'].*) > 15]" -> $ /? GreaterThan(
        Count(JsonPathNodes(`@` / "items" / Wildcard)),
        LiteralNumber(15)
      ),
      s"$$[?match(@.name, '.*foo.*')]" -> $ /? Match(
        JsonPathValue(`@` / "name"),
        LiteralString(".*foo.*")
      ),
      s"$$[?search(@.name, 'Jane')]" -> $ /? Search(
        JsonPathValue(`@` / "name"),
        LiteralString("Jane")
      )
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.default.parse(input).value should be(expected)
    }
  }

  it should "parse nested functions" in {
    JsonPathParser.default
      .parse(s"$$[?(length(value(@.foo)) > 3)]")
      .value should be(
      $ /? GreaterThan(
        Length(
          Value(JsonPathValue(`@` / "foo"))
        ),
        LiteralNumber(3)
      )
    )
  }

  it should "fail on unrecognized function extensions" in {
    val failure = JsonPathParser.default.parse(s"$$[?foobar(@.foo) > 3]").failed
    failure.index should be(3)
    failure.message should include("foobar")
  }

  it should "fail if there are too few for a function extension arguments" in {
    val failure = JsonPathParser.default.parse(s"$$[?length() > 3]").failed
    failure.index should be(3)
    failure.message should (include("length") and include(
      "expects 1 argument, but got 0"
    ))
  }
}
