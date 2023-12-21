package com.quincyjo.jsonpath

trait JsonSupport[Json] {
  
  def Null: Json

  def asObject(json: Json): Option[Map[String, Json]]
  def asArray(json: Json): Option[Vector[Json]]
  def asString(json: Json): Option[String]
  def asBoolean(json: Json): Option[Boolean]
  def asNumber(json: Json): Option[BigDecimal]

  def isObject(json: Json): Boolean
  def isArray(json: Json): Boolean
  def isString(json: Json): Boolean
  def isBoolean(json: Json): Boolean
  def isNumber(json: Json): Boolean

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

      def isObject: Boolean = jsonSupport.isObject(json)
      def isArray: Boolean = jsonSupport.isArray(json)
      def isString: Boolean = jsonSupport.isString(json)
      def isBoolean: Boolean = jsonSupport.isBoolean(json)
      def isNumber: Boolean = jsonSupport.isNumber(json)

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
    }
  }
}
