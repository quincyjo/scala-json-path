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
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class JsonPathSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks {

  "toString" should "encode absolute paths with a leading $" in {
    val jsonPath = JsonPath.$ / "foobar"

    jsonPath.toString should be("$['foobar']")
  }

  it should "encode relative paths a leading @" in {
    val jsonPath = JsonPath.`@` / "foobar" / "deadbeef"

    jsonPath.toString should be("@['foobar']['deadbeef']")
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

  "Script" should "encode with parentheses" in {
    val expression = GreaterThan(
      JsonPathValue(`@` / "foobar"),
      LiteralNumber(3)
    )
    val scriptExpression = Script(expression)

    scriptExpression.toString should be(s"($expression)")
  }
}
