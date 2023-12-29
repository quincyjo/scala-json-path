/*
 * Copyright 2023 Typelevel
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

import com.quincyjo.jsonpath.JsonPath.Attribute
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AttributeSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with TableDrivenPropertyChecks {

  "isSimple" should "determine if the name is dot-chainable" in {
    val cases = Table(
      "right" -> "expected",
      "foobar" -> true,
      "abc123" -> true,
      "1" -> false,
      "123abc" -> false
    )

    forAll(cases) { case (name, expected) =>
      Attribute(name).isSimple should be(expected)
    }
  }

  "quotedName" should "wrap simple names in double quotes" in {
    Attribute("foobar").quotedName should be("\"foobar\"")
  }

  it should "escape double quotes in names" in {
    Attribute("\"foobar\"").quotedName should be("\"\\\"foobar\\\"\"")
  }

  "toString" should "be just the right for simple names" in {
    Attribute("foobar").toString should be("foobar")
  }

  it should "be quoted for complex names" in {
    Attribute("\"foobar\"").toString should be("\"\\\"foobar\\\"\"")
  }
}
