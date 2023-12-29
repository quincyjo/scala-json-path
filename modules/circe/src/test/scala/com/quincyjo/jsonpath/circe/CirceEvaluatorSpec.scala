package com.quincyjo.jsonpath.circe

import com.quincyjo.jsonpath.Expression._
import com.quincyjo.jsonpath.JsonPath._
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CirceEvaluatorSpec extends AnyFlatSpecLike with Matchers {

  private val apple = Json.obj(
    "label" -> "Apple".asJson,
    "price" -> 2.asJson,
    "quantity" -> 15.asJson
  )
  private val banana = Json.obj(
    "label" -> "Banana".asJson,
    "price" -> 1.asJson,
    "quantity" -> 23.asJson
  )
  private val json = Json.obj(
    "products" -> Json.arr(
      apple,
      banana,
      Json.obj(
        "label" -> "Dinner Set".asJson,
        "price" -> 30.asJson,
        "quantity" -> 2.asJson
      )
    )
  )

  "evaluate" should "behave" in {
    val jsonPath = $ / "products" / Filter(
      LessThanOrEqualTo(
        JsonPathValue(`@` / "price"),
        JsonNumber(10)
      )
    )

    CirceEvaluator.evaluate(
      jsonPath,
      json
    ) should contain theSameElementsAs Seq(
      apple,
      banana
    )
  }
}
