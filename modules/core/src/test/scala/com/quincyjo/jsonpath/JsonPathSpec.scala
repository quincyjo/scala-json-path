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
import com.quincyjo.jsonpath.JsonPath._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathSpec
    extends AnyFlatSpecLike
    with Matchers
    with Inside
    with TableDrivenPropertyChecks {

  "toString" should "encode absolute paths with a leading $" in {
    val jsonPath = JsonPath.$ / "foobar"

    jsonPath.toString should be("$['foobar']")
  }

  it should "encode relative paths a leading @" in {
    val jsonPath = JsonPath.`@` / "foobar" / "deadbeef"

    jsonPath.toString should be("@['foobar']['deadbeef']")
  }

  "isAbsolute" should "be true if the root is $" in {
    val jsonPath = JsonPath.$ / "foobar"

    jsonPath.isAbsolute should be(true)
  }

  it should "be false if the root is not $" in {
    val jsonPath = JsonPath.`@` / "foobar"

    jsonPath.isAbsolute should be(false)
  }

  "isRelative" should "be true if the root is @" in {
    val jsonPath = JsonPath.`@` / "foobar" / "deadbeef"

    jsonPath.isRelative should be(true)
  }

  it should "be false if the root is not @" in {
    val jsonPath = JsonPath.$ / "foobar" / "deadbeef"

    jsonPath.isRelative should be(false)
  }

  "/" should "maintain singular queries if the selector is singular" in {
    val selector: Selector = Attribute("foobar")

    (JsonPath.$ / selector).isSingular should be(true)
  }

  it should "shift to a non-singular query if it is a multiselector" in {
    val selector: Selector = Slice.take(5)

    (JsonPath.$ / selector).isSingular should be(false)
  }

  "*/" should "apply the selector via recursive descent" in {
    JsonPath.$ */ Wildcard should be(
      JsonPath.$ appended RecursiveDescent(Wildcard)
    )
  }

  "isEmpty" should "be true if the path has no segments" in {
    JsonPath.$.isEmpty should be(true)
    JsonPath.`@`.isEmpty should be(true)
  }

  it should "be false if there is at least one segment" in {
    (JsonPath.$ / "foobar").isEmpty should be(false)
  }

  "size" should "return the number of segments" in {
    val cases = Table(
      "query" -> "expected size",
      JsonPath.$ / "foobar" -> 1,
      JsonPath.$ / "foobar" / "deadbeef" -> 2,
      JsonPath.$ */ Wildcard / 0 / "deadbeef" -> 3,
      JsonPath.$ /? JsonPathNodes(JsonPath.`@` / "check") -> 1
    )

    forAll(cases) { case (query, expectedSize) =>
      query.size should be(expectedSize)
    }
  }

  "length" should "be synonymous with size" in {
    val cases = Table(
      "query",
      JsonPath.$ / "foobar",
      JsonPath.$ / "foobar" / "deadbeef",
      JsonPath.$ */ Wildcard / 0 / "deadbeef",
      JsonPath.$ /? JsonPathNodes(JsonPath.`@` / "check")
    )

    forAll(cases) { query =>
      query.length should be(query.size)
    }
  }

  "hasParent" should "be true for paths that have a parent" in {
    val jsonPath = JsonPath.$ / "foobar"

    jsonPath.hasParent should be(true)
  }

  it should "be false for paths that do not have a parent" in {
    val jsonPath = JsonPath.$

    jsonPath.hasParent should be(false)
  }

  "resolve" should "append relative paths" in {
    val rootJsonPath = JsonPath.$ / "foobar"
    val relativeJsonPath = JsonPath.`@` / "deadbeef"

    rootJsonPath.resolve(relativeJsonPath) should be(
      JsonPath.$ / "foobar" / "deadbeef"
    )
  }

  it should "take the other path if not relative" in {
    val rootJsonPath = JsonPath.$ / "foobar"
    val otherJsonPath = JsonPath.$ / "deadbeef"

    rootJsonPath.resolve(otherJsonPath) should be(otherJsonPath)
  }

  it should "be the base path if the other path is empty" in {
    val rootJsonPath = JsonPath.$ / "foobar"

    rootJsonPath.resolve(JsonPath.`@`) should be(rootJsonPath)
  }

  it should "become a Query if either is non-singular" in {
    val rootJsonPath = JsonPath.$ / "foobar"
    val relativeJsonPath = JsonPath.`@` */ Wildcard

    inside(rootJsonPath.resolve(relativeJsonPath)) { case query: Query =>
      query.segments should contain theSameElementsInOrderAs Seq(
        Child("foobar"),
        RecursiveDescent(Wildcard)
      )
    }
  }

  "resolveSibling" should "resolve relative paths against the parent path" in {
    val rootJsonPath = JsonPath.$ / "foobar"
    val relativeJsonPath = JsonPath.`@` / "deadbeef"

    rootJsonPath.resolveSibling(relativeJsonPath) should be(
      JsonPath.$ / "deadbeef"
    )
  }

  it should "take the other path if not relative" in {
    val rootJsonPath = JsonPath.$ / "foobar"
    val otherJsonPath = JsonPath.$ / "deadbeef"

    rootJsonPath.resolveSibling(otherJsonPath) should be(otherJsonPath)
  }

  it should "take the other path if this path has no parent" in {
    val rootJsonPath = JsonPath.$
    val otherJsonPath = JsonPath.`@` / "deadbeef"

    rootJsonPath.resolveSibling(otherJsonPath) should be(otherJsonPath)
  }

  "Property" should "encode indices with brackets" in {
    Child(1).toString should be("[1]")
  }

  it should "encode attributes with single quote brackets" in {
    Child("name").toString should be("['name']")
  }

  it should "encode ambiguous attributes with quotes" in {
    Child("1").toString should be("['1']")
  }

  it should "encode complex attributes with quotes in bracket notation" in {
    Child("Foobar and deadbeef").toString should be(
      "[\'Foobar and deadbeef\']"
    )
  }

  it should "escape leave double quotes" in {
    Child("\"Proper Noun\"").toString should be("['\"Proper Noun\"']")
  }

  "RecursiveDescent" should "encode indices with brackets" in {
    RecursiveDescent(1).toString should be("..[1]")
  }

  it should "encode attributes with bracket notation" in {
    RecursiveDescent("name").toString should be("..['name']")
  }

  it should "encode complex attributes with quotes in bracket notation" in {
    RecursiveDescent("Foobar and deadbeef").toString should be(
      "..['Foobar and deadbeef']"
    )
  }

  "Slice" should "encode a complete slice" in {
    Slice(1, 2, 3).toString should be("1:2:3")
  }

  it should "encode a first N slice" in {
    Slice.take(5).toString should be(":5")
  }

  it should "encode a last N slice" in {
    Slice.drop(-5).toString should be("-5:")
  }

  it should "encode a N to M slice" in {
    Slice(1, 2).toString should be("1:2")
  }

  it should "encode a step slice" in {
    Slice.everyN(3).toString should be("::3")
  }

  "Union" should "encode as a comma deliminated list" in {
    Union(1, 2, 3).toString should be("1,2,3")
  }

  "Filter" should "encode with parentheses and leading '?'" in {
    val expression = GreaterThan(
      JsonPathValue(`@` / "foobar"),
      LiteralNumber(3)
    )
    val filterExpression = Filter(expression)

    filterExpression.toString should be(s"?($expression)")
  }

  behavior of "SingularQuery"

  "appended" should "maintain singularity if the segment is singular" in {
    val segment: JsonPathSegment = Child("foobar")

    JsonPath.$.appended(segment).isSingular should be(true)
  }

  "appendedAll" should "maintain singularity if the segments are singular" in {
    val segments: Iterable[JsonPathSegment] = Seq(Child("foobar"))

    JsonPath.$.appendedAll(segments).isSingular should be(true)
  }

  it should "maintain singularity the segments are explicitly singular" in {
    val segments: Iterable[Child] = Seq(Child("foobar"))

    JsonPath.$.appendedAll(segments).isSingular should be(true)
  }

  "prepended" should "maintain singularity if the segment is singular" in {
    val segment: JsonPathSegment = Child("foobar")

    JsonPath.$.prepended(segment).isSingular should be(true)
  }

  it should "loose singularity if the segment is plural" in {
    val segment: JsonPathSegment = Children(Wildcard)

    JsonPath.$.prepended(segment).isSingular should be(false)
  }

  it should "maintain singularity the segments are explicitly singular" in {
    val segment: Child = Child("foobar")

    JsonPath.$.prepended(segment).isSingular should be(true)
  }

  "prependedAll" should "maintain singularity if the segments are singular" in {
    val segments: Iterable[JsonPathSegment] = Seq(Child("foobar"))

    JsonPath.$.prependedAll(segments).isSingular should be(true)
  }

  it should "maintain singularity the segments are explicitly singular" in {
    val segments: Iterable[Child] = Seq(Child("foobar"))

    JsonPath.$.prependedAll(segments).isSingular should be(true)
  }

  "drops and takes" should "maintain singularity" in {
    val query = JsonPath.$ / "foobar" / "deadbeef" / 1

    query.drop(1).isSingular should be(true)
    query.take(1).isSingular should be(true)
    query.dropRight(1).isSingular should be(true)
    query.takeRight(1).isSingular should be(true)
  }

  behavior of "Query"

  "appended" should "maintain plurality if the segment is singular" in {
    val segment: JsonPathSegment = Child("foobar")

    (JsonPath.$ */ Wildcard appended segment).isSingular should be(false)
  }

  "appendedAll" should "maintain plurality" in {
    val segments: Iterable[JsonPathSegment] = Seq(Child("foobar"))

    (JsonPath.$ */ Wildcard appendedAll segments).isSingular should be(false)
  }

  "prepended" should "maintain plurality" in {
    val segment: JsonPathSegment = Child("foobar")

    (JsonPath.$ */ Wildcard prepended segment).isSingular should be(false)
  }

  "prependedAll" should "maintain plurality if the segments are singular" in {
    val segments: Iterable[JsonPathSegment] = Seq(Child("foobar"))

    (JsonPath.$ */ Wildcard prependedAll segments).isSingular should be(false)
  }

  "drop" should "become singular if able" in {
    (JsonPath.$ */ Wildcard drop 1).isSingular should be(true)
  }

  "take" should "become singular if able" in {
    (JsonPath.$ / "foobar" */ Wildcard take 1).isSingular should be(true)
  }

  "dropRight" should "become singular if able" in {
    (JsonPath.$ */ Wildcard dropRight 1).isSingular should be(true)
  }

  "takeRight" should "become singular if able" in {
    (JsonPath.$ */ Wildcard / "foobar" takeRight 1).isSingular should be(true)
  }
}
