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

package com.quincyjo.jsonpath.extensions

import com.quincyjo.jsonpath.Expression.{
  LiteralBoolean,
  LiteralNumber,
  LiteralString
}
import com.quincyjo.jsonpath.JsonBean
import com.quincyjo.jsonpath.JsonBean.JsonBeanEvaluator
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class MatchSpec
    extends AnyFlatSpecLike
    with Matchers
    with OptionValues
    with TableDrivenPropertyChecks {

  "Match" should "be true if the string matches the regex" in {
    val cases = Table(
      ("given string", "given regex", "is a match"),
      ("foo", "f..", true),
      ("foobar", "f.*", true),
      ("foobar", "f..", false),
      ("1974-05-01", "1974-05-..", true),
      ("1974-06-01", "1974-05-..", false)
    )

    forAll(cases) { case (givenString, givenRegex, isAMatch) =>
      Match(LiteralString(givenString), LiteralString(givenRegex))(
        JsonBeanEvaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(isAMatch)
    }
  }

  it should "be false if the value is not a string" in {
    val cases = Table(
      "json",
      LiteralBoolean(false),
      LiteralNumber(42)
    )

    forAll(cases) { json =>
      Match(json, LiteralString(".*"))(
        JsonBeanEvaluator,
        JsonBean.Null,
        JsonBean.Null
      ) should be(false)
    }
  }

  it should "be false if the regex is not valid" in {
    Match(LiteralString("foo"), LiteralString("[*"))(
      JsonBeanEvaluator,
      JsonBean.Null,
      JsonBean.Null
    ) should be(false)
  }
}
