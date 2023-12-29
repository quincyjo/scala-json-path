package com.quincyjo.jsonpath.circe

import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class CirceSupportSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks {

  "asObject" should "only be defined for objects" in {
    val cases = Table[Json, Option[Map[String, Json]]](
      ("given", "expected"),
      (Json.Null, None),
      (Json.fromString("foo"), None),
      (Json.fromBoolean(true), None),
      (Json.fromInt(42), None),
      (Json.arr(Json.True), None),
      (Json.obj(), Some(Map.empty)),
      (
        Json.obj("foo" -> Json.fromString("bar")),
        Some(Map("foo" -> Json.fromString("bar")))
      )
    )

    forAll(cases) { (json, expected) =>
      CirceSupport.asObject(json) should be(expected)
    }
  }
}
