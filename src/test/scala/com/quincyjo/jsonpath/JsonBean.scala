package com.quincyjo.jsonpath

sealed trait JsonBean

object JsonBean {

  final val True: JBoolean = JBoolean(true)
  final val False: JBoolean = JBoolean(false)
  final val Null = JNull

  def apply(value: JsonValueWrapper): JsonBean = value.value
  def arr(values: JsonValueWrapper*): JsonBean = JArray(values.map(_.value).toVector)
  def fromValues(values: Iterable[JsonBean]): JsonBean = JArray(values.toVector)
  def obj(values: (String, JsonValueWrapper)*): JsonBean = JObject(values.toMap.view.mapValues(_.value).toMap)
  def fromAttributes(values: Iterable[(String, JsonBean)]): JsonBean = JObject(values.toMap)
  def string(string: String): JsonBean = JString(string)
  def number(int: Int): JsonBean = JNumber(int)
  def boolean(boolean: Boolean): JsonBean = JBoolean(boolean)

  final case class JObject(underlying: Map[String, JsonBean]) extends JsonBean {

    override def toString: String =
      s"""{ ${underlying.map {
        case (key, value) => s""""$key": $value"""
      }.mkString(", ")} }"""
  }

  final case class JArray(values: Vector[JsonBean]) extends JsonBean {

    override def toString: String =
      s"""[ ${values.map(_.toString).mkString(", ")} ]"""
  }

  final case class JBoolean(value: Boolean) extends JsonBean {

    override def toString: String =
      if (value) "true" else "false"
  }

  final case class JNumber(value: BigDecimal) extends JsonBean {

    override def toString: String =
      if (value.isValidInt) value.toInt.toString
      else value.toString()
  }

  final case class JString(value: String) extends JsonBean {

    override def toString: String =
      s"\"$value\""
  }

  case object JNull extends JsonBean {

    override def toString: String = "null"
  }

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
      json match {
        case JObject(underlying) => jsonObject(underlying)
        case JArray(values) => jsonArray(values)
        case JBoolean(value) => jsonBoolean(value)
        case JNumber(value) => jsonNumber(value)
        case JString(value) => jsonString(value)
        case JNull => ifNull
      }
  }

  sealed trait JsonValueWrapper {
    def value: JsonBean
  }

  private case class JsonValueWrapperImpl(field: JsonBean)
    extends JsonValueWrapper {
    override def value: JsonBean = field
  }

  import scala.language.implicitConversions

  implicit def toJsFieldJsonBeanWrapper[T](field: T)(
    implicit w: JsonValueMagnet[T]
  ): JsonValueWrapper =
    JsonValueWrapperImpl(w(field))

  sealed trait JsonValueMagnet[-T] {
    def apply(t: T): JsonBean
  }

  object JsonValueMagnet {

    implicit case object JsonBeanMagnet extends JsonValueMagnet[JsonBean] {
      override def apply(t: JsonBean): JsonBean = t
    }

    implicit case object StringMagnet extends JsonValueMagnet[String] {
      override def apply(t: String): JsonBean = string(t)
    }

    implicit case object NumberMagnet extends JsonValueMagnet[Int] {
      override def apply(t: Int): JsonBean = number(t)
    }

    implicit case object BooleanMagnet extends JsonValueMagnet[Boolean] {
      override def apply(t: Boolean): JsonBean = boolean(t)
    }
  }
}
