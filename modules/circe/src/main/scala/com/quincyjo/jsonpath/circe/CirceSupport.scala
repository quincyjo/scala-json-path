package com.quincyjo.jsonpath.circe

import com.quincyjo.jsonpath.JsonSupport
import io.circe.Json

trait CirceSupport {

  implicit val circeSupport: JsonSupport[Json] = CirceSupport
}

object CirceSupport extends JsonSupport[Json] {

  override def string(string: String): Json =
    Json.fromString(string)

  override def number(bigDecimal: BigDecimal): Json =
    Json.fromBigDecimal(bigDecimal)

  override def boolean(boolean: Boolean): Json =
    if (boolean) Json.True else Json.False

  override def Null: Json =
    Json.Null

  override def asObject(json: Json): Option[Map[String, Json]] =
    json.asObject.map(_.toMap)

  override def asArray(json: Json): Option[Vector[Json]] =
    json.asArray

  override def asString(json: Json): Option[String] =
    json.asString

  override def asBoolean(json: Json): Option[Boolean] =
    json.asBoolean

  override def asNumber(json: Json): Option[BigDecimal] =
    json.asNumber.flatMap(_.toBigDecimal)

  override def asNull(json: Json): Option[Unit] =
    json.asNull

  override def isObject(json: Json): Boolean =
    json.isObject

  override def isArray(json: Json): Boolean =
    json.isArray

  override def isString(json: Json): Boolean =
    json.isString

  override def isBoolean(json: Json): Boolean =
    json.isBoolean

  override def isNumber(json: Json): Boolean =
    json.isNumber

  override def isNull(json: Json): Boolean =
    json.isNull

  override def fold[B](json: Json)(
      ifNull: => B,
      jsonBoolean: Boolean => B,
      jsonNumber: BigDecimal => B,
      jsonString: String => B,
      jsonArray: Vector[Json] => B,
      jsonObject: Map[String, Json] => B
  ): B =
    json.fold(
      ifNull,
      jsonBoolean,
      _ => asNumber(json).fold(ifNull)(jsonNumber),
      jsonString,
      jsonArray,
      obj => jsonObject(obj.toMap)
    )
}
