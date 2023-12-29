package com.quincyjo.jsonpath.parser

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathReaderSpec
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
      "$..author" -> $ / RecursiveDescent(Attribute("author")),
      "$.store.*" -> $ / "store" / Wildcard,
      "$.store..price" -> $ / "store" / RecursiveDescent(Attribute("price")),
      "$..book[2]" -> $ / RecursiveDescent(Attribute("book")) / 2,
      "$..book[-1:]" -> $ / RecursiveDescent(Attribute("book")) / Slice
        .takeRight(1),
      "$..book[0,1]" -> $ / RecursiveDescent(Attribute("book")) / Union(0, 1),
      "$..book[:2]" -> $ / RecursiveDescent(Attribute("book")) / Slice.take(2),
      "$..*" -> $ / RecursiveDescent(Wildcard)
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.parse(input).value should be(expected)
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
      JsonPathParser.parse(input).value should be(expected)
    }
  }

  it should "handle quoted strings with escaped nested quotes" in {
    val cases = Table(
      "input" -> "expected",
      "$['ain\\'t that neat']" -> $ / "ain't that neat",
      "$[\"\\\"Proper Noun\\\"\"]" -> $ / "\"Proper Noun\""
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.parse(input).value should be(expected)
    }
  }

  it should "parse expressions according to the configured expression parser" in {
    val cases = Table(
      "input" -> "expected",
      "$[(@.foobar>3)]" -> $ / Script(
        GreaterThan(JsonPathValue(`@` / "foobar"), JsonNumber(3))
      ),
      "$[?(!!@.length >= 5 && @[5].isValid)]" -> $ / Filter(
        And(
          GreaterThanOrEqualTo(
            Not(Not(JsonPathValue(`@` / "length"))),
            JsonNumber(5)
          ),
          JsonPathValue(`@` / 5 / "isValid")
        )
      )
    )

    forAll(cases) { (input, expected) =>
      JsonPathParser.parse(input).value should be(expected)
    }
  }

  "take" should "read up to the first parse error" in {
    val cases = Table(
      ("input", "JsonPath", "raw"),
      ("$.foobar", $ / "foobar", "$.foobar"),
      ("$.foobar > 5", $ / "foobar", "$.foobar "),
      ("$ > 5", $, "$ "),
      ("@[:-1]", `@` / Slice.dropRight(1), "@[:-1]"),
      ("['foobar']", JsonPath.empty / "foobar", "['foobar']")
    )

    forAll(cases) { (input, jsonPath, raw) =>
      val result = JsonPathParser.take(input).value
      result.value should be(jsonPath)
      result.raw should be(raw)
    }
  }
}
