package com.quincyjo.jsonpath

import scala.util.Try

trait JsonSupport[Json] {

  def string(string: String): Json
  def number(bigDecimal: BigDecimal): Json
  def boolean(boolean: Boolean): Json

  def Null: Json

  def asObject(json: Json): Option[Map[String, Json]]
  def asArray(json: Json): Option[Vector[Json]]
  def asString(json: Json): Option[String]
  def asBoolean(json: Json): Option[Boolean]
  def asNumber(json: Json): Option[BigDecimal]
  def asNull(json: Json): Option[Unit]

  def isObject(json: Json): Boolean
  def isArray(json: Json): Boolean
  def isString(json: Json): Boolean
  def isBoolean(json: Json): Boolean
  def isNumber(json: Json): Boolean
  def isNull(json: Json): Boolean

  final def isAtomic(json: Json): Boolean = !isObject(json) && !isArray(json)
  final def isAssociative(json: Json): Boolean = isObject(json) || isArray(json)

  def fold[B](json: Json)(
      ifNull: => B,
      jsonBoolean: Boolean => B,
      jsonNumber: BigDecimal => B,
      jsonString: String => B,
      jsonArray: Vector[Json] => B,
      jsonObject: Map[String, Json] => B
  ): B

  def arrayOrObject[B](json: Json)(
      orElse: => B,
      jsonArray: Vector[Json] => B,
      jsonObject: Map[String, Json] => B
  ): B = fold(json)(
    orElse,
    _ => orElse,
    _ => orElse,
    _ => orElse,
    jsonArray,
    jsonObject
  )

  /** Coerces the given JSON right into a number. If the result is [[None]],
    * then the right is not a number (javascript NaN).
    * @param json
    *   The json to coerce.
    * @return
    *   The coerced number or NaN.
    */
  def coerceToNumber(json: Json): Option[BigDecimal] =
    fold(json)(
      Some(0),
      boolean => Some(if (boolean) 1 else 0),
      Some(_),
      string =>
        Option
          .when(string.isEmpty)(BigDecimal(0))
          .orElse(
            Try(BigDecimal(string)).toOption
          ),
      {
        case arr if arr.isEmpty   => Some(0)
        case arr if arr.size == 1 => arr.headOption.flatMap(coerceToNumber)
        case _                    => None
      },
      _ => None
    )

  /** Coerces the given JSON right into a string.
    * @param json
    *   The json to coerce.
    * @return
    *   The coerced string.
    */
  def coerceToString(json: Json): String =
    fold(json)(
      "null",
      if (_) "true" else "false",
      _.toString(),
      identity,
      _.map(coerceToString).mkString(","),
      _ => "[object Object]"
    )

  /** Coerces the given JSON right into a boolean.
    * @param json
    *   The json to coerce.
    * @return
    *   The coerced boolean.
    */
  def coerceToBoolean(json: Json): Boolean =
    fold(json)(
      false,
      identity,
      _ != 0,
      _.nonEmpty,
      _ => true,
      _ => true
    )

  def coerceToPrimitive(json: Json): Json =
    if (isAssociative(json)) string(coerceToString(json))
    else json

  def areSameType(a: Json, b: Json): Boolean =
    fold(a)(
      isNull(b),
      _ => isBoolean(b),
      _ => isNumber(b),
      _ => isString(b),
      _ => isAssociative(b),
      _ => isAssociative(b)
    )

  /** Applies type converesion to coerce both of the given JSON values into the
    * same type. If they cannot be coerced into the same type, meaning that one
    * or b oth of the values produces NaN during the conversion, the nothing is
    * returned. Otherwise, two JSONs of directly comparable type sare returned.
    * @param a
    *   The first JSON.
    * @param b
    *   The second JSON.
    * @return
    *   {@code a} and {@code b} coerced into the same type if able.
    */
  def convertTypes(a: Json, b: Json): Option[(Json, Json)] =
    if (areSameType(a, b) || isNull(a) || isNull(b))
      Some(a -> b)
    else {
      val newA =
        if (isAssociative(a)) coerceToPrimitive(a)
        else a
      val newB =
        if (isAssociative(b)) coerceToPrimitive(b)
        else b
      if (areSameType(newA, newB))
        Some(newA -> newB)
      else
        coerceToNumber(newA)
          .map(number)
          .zip(coerceToNumber(newB).map(number))
    }
}

object JsonSupport {

  object Implicits {

    implicit class JsonSupportOps[Json](json: Json)(implicit
        jsonSupport: JsonSupport[Json]
    ) {

      def asObject: Option[Map[String, Json]] = jsonSupport.asObject(json)
      def asArray: Option[Vector[Json]] = jsonSupport.asArray(json)
      def asString: Option[String] = jsonSupport.asString(json)
      def asBoolean: Option[Boolean] = jsonSupport.asBoolean(json)
      def asNumber: Option[BigDecimal] = jsonSupport.asNumber(json)
      def asNull: Option[Unit] = jsonSupport.asNull(json)

      def isObject: Boolean = jsonSupport.isObject(json)
      def isArray: Boolean = jsonSupport.isArray(json)
      def isString: Boolean = jsonSupport.isString(json)
      def isBoolean: Boolean = jsonSupport.isBoolean(json)
      def isNumber: Boolean = jsonSupport.isNumber(json)
      def isNull: Boolean = jsonSupport.isNull(json)

      def isAtomic: Boolean = jsonSupport.isAtomic(json)
      def isAssociative: Boolean = jsonSupport.isAssociative(json)

      def fold[B](
          ifNull: => B,
          jsonBoolean: Boolean => B,
          jsonNumber: BigDecimal => B,
          jsonString: String => B,
          jsonArray: Vector[Json] => B,
          jsonObject: Map[String, Json] => B
      ): B = jsonSupport.fold(json)(
        ifNull,
        jsonBoolean,
        jsonNumber,
        jsonString,
        jsonArray,
        jsonObject
      )

      def arrayOrObject[B](
          orElse: => B,
          jsonArray: Vector[Json] => B,
          jsonObject: Map[String, Json] => B
      ): B = jsonSupport.arrayOrObject(json)(orElse, jsonArray, jsonObject)

      def coerceToNumber: Option[BigDecimal] =
        jsonSupport.coerceToNumber(json)

      def coerceToString: String =
        jsonSupport.coerceToString(json)

      def coerceToBoolean: Boolean =
        jsonSupport.coerceToBoolean(json)

      def coerceToPrimitive: Json =
        jsonSupport.coerceToPrimitive(json)

      def isSameType(that: Json): Boolean =
        jsonSupport.areSameType(json, that)
    }
  }
}
