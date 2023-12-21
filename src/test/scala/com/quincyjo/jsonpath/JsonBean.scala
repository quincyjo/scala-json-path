package com.quincyjo.jsonpath

sealed trait JsonBean {

}

object JsonBean {

  final val True: JBoolean = JBoolean(true)
  final val False: JBoolean = JBoolean(false)
  final val Null = JNull

  def arr(values: JsonBean*): JsonBean = JArray(values.toVector)
  def obj(values: (String, JsonBean)*): JsonBean = JObject(values.toMap)
  def string(string: String): JsonBean = JString(string)
  def number(int: Int): JsonBean = JNumber(int)
  def boolean(boolean: Boolean): JsonBean = JBoolean(boolean)

  final case class JObject(underlying: Map[String, JsonBean]) extends JsonBean

  final case class JArray(values: Vector[JsonBean]) extends JsonBean

  final case class JBoolean(value: Boolean) extends JsonBean

  final case class JNumber(value: BigDecimal) extends JsonBean

  final case class JString(value: String) extends JsonBean

  case object JNull extends JsonBean

  implicit final val jsonBeanSupport: JsonSupport[JsonBean] = new JsonSupport[JsonBean] {
    
    final val Null: JsonBean = JNull

    override def asObject(json: JsonBean): Option[Map[String, JsonBean]] = json match {
      case JObject(underlying) => Some(underlying)
      case _ => None
    }

    override def asArray(json: JsonBean): Option[Vector[JsonBean]] = json match {
      case JArray(values) => Some(values)
      case _ => None
    }

    override def asString(json: JsonBean): Option[String] = json match {
      case JString(value) => Some(value)
      case _ => None
    }

    override def asBoolean(json: JsonBean): Option[Boolean] = json match {
      case JBoolean(value) => Some(value)
      case _ => None
    }

    override def asNumber(json: JsonBean): Option[BigDecimal] = json match {
      case JNumber(value) => Some(value)
      case _ => None
    }

    override def isObject(json: JsonBean): Boolean = json match {
      case JObject(_) => true
      case _ => false
    }

    override def isArray(json: JsonBean): Boolean = json match {
      case JArray(_) => true
      case _ => false
    }

    override def isString(json: JsonBean): Boolean = json match {
      case JString(_) => true
      case _ => false
    }

    override def isBoolean(json: JsonBean): Boolean = json match {
      case JBoolean(_) => true
      case _ => false
    }

    override def isNumber(json: JsonBean): Boolean = json match {
      case JNumber(_) => true
      case _ => false
    }

    override def fold[B](json: JsonBean)(ifNull: => B, jsonBoolean: Boolean => B, jsonNumber: BigDecimal => B, jsonString: String => B, jsonArray: Vector[JsonBean] => B, jsonObject: Map[String, JsonBean] => B): B =
      json match
        case JObject(underlying) => jsonObject(underlying)
        case JArray(values) => jsonArray(values)
        case JBoolean(value) => jsonBoolean(value)
        case JNumber(value) => jsonNumber(value)
        case JString(value) => jsonString(value)
        case JNull => ifNull
  }
}
