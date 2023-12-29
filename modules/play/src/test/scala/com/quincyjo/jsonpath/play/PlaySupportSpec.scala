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

package com.quincyjo.jsonpath.play

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json._

class PlaySupportSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks {

  "asObject" should "only be defined for objects" in {
    val cases = Table[JsValue, Option[Map[String, JsValue]]](
      ("given", "expected"),
      (JsNull, None),
      (JsString("foo"), None),
      (JsBoolean(true), None),
      (JsNumber(42), None),
      (Json.arr(true), None),
      (Json.obj(), Some(Map.empty)),
      (
        Json.obj("foo" -> "bar"),
        Some(Map("foo" -> JsString("bar")))
      )
    )

    forAll(cases) { case (json, expected) =>
      PlaySupport.asObject(json) should be(expected)
    }
  }
}
