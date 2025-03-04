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

import com.quincyjo.jsonpath.parser.util.BalancedExpressionReader.ExpressionGroup
import com.quincyjo.jsonpath.parser.util.BalancedExpressionReader.ExpressionGroup._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BalancedExpressionReaderSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks
    with OptionValues {

  "isBalanced" should "handle basic asymmetrical groups" in {
    val groups = ExpressionGroup.all.collect {
      case assymetric: AsymmetricalExpressionGroup =>
        assymetric
    }

    for (group <- groups) yield {
      val start = group.start
      val end = group.end
      val cases = Table(
        ("input", "expected"),
        (s"${start}${end}", true),
        (s"${start}foobar${end}", true),
        (s"${start}foobar${start}deadbeef${end}${end}", true),
        (s"foorbar${start}${end}", true),
        (s"${start}", false),
        (s"${end}", false),
        (s"${start}foobar", false),
        (s"${start}foobardeadbeef${end}${end}", false),
        (s"foorbar${start}", false)
      )

      forAll(cases) { case (input, expected) =>
        BalancedExpressionReader(input).isBalanced should be(expected)
      }
    }
  }

  it should "handle basic escape groups" in {
    val cases = Table(
      "input" -> "expected",
      "'foobar'" -> true,
      "'fo[\"{o(bar'" -> true,
      "'foobar\\'s'" -> true
    )

    forAll(cases) { case (input, expected) =>
      BalancedExpressionReader(input).isBalanced should be(expected)
    }
  }

  "takeGroup" should "take simple groups" in {
    val cases = Table(
      "input" -> "expected",
      "()" -> "()",
      "(foobar)" -> "(foobar)",
      "(foobar)deadbeef" -> "(foobar)",
      "(foobar())deadbeef" -> "(foobar())",
      "[(foobar())deadbeef]" -> "[(foobar())deadbeef]"
    )

    forAll(cases) { case (input, expected) =>
      BalancedExpressionReader(input).takeGroup should be(expected)
    }
  }

  "takeUntil" should "take until the given predicate" in {
    val cases = Table(
      "input" -> "expected",
      "?foo > 5]" -> "?foo > 5",
      "?foo > 5, @.foo.bar]" -> "?foo > 5",
      "?foo > @.foo[?(), ?()]]" -> "?foo > @.foo[?(), ?()]",
      "?@.foo[5] && $.evaluate]" -> "?@.foo[5] && $.evaluate"
    )

    forAll(cases) { case (input, expected) =>
      BalancedExpressionReader(input).takeUntil(char =>
        char == ',' || char == ']'
      ) should be(expected)
    }
  }
}
