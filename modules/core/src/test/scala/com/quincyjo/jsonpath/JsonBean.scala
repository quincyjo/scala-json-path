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

package com.quincyjo.jsonpath

import com.quincyjo.braid.Braid

sealed trait JsonBean

object JsonBean {

  final val True: JBoolean = JBoolean(true)
  final val False: JBoolean = JBoolean(false)
  final val Null: JNull.type = JNull

  def apply(value: JsonValueWrapper): JsonBean = value.value

  def arr(values: JsonValueWrapper*): JArray = JArray(
    values.map(_.value).toVector
  )

  def fromValues(values: Iterable[JsonBean]): JArray = JArray(values.toVector)

  def obj(values: (String, JsonValueWrapper)*): JObject = JObject(
    values.toMap.view.mapValues(_.value).toMap
  )

  def fromAttributes(values: Iterable[(String, JsonBean)]): JObject = JObject(
    values.toMap
  )

  def string(string: String): JString = JString(string)

  def number(int: Int): JNumber = JNumber(int)

  def boolean(boolean: Boolean): JBoolean = JBoolean(boolean)

  final case class JObject(underlying: Map[String, JsonBean]) extends JsonBean {

    override def toString: String =
      s"""{ ${underlying
        .map { case (key, value) =>
          s""""$key": $value"""
        }
        .mkString(", ")} }"""
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

    override def toString: String = s"""\"$value\""""
  }

  case object JNull extends JsonBean {

    override def toString: String = "null"
  }

  implicit final val jsonBeanBraid: Braid[JsonBean] =
    new Braid[JsonBean] {

      override def fromString(string: String): JsonBean =
        JString(string)

      override def fromBigDecimal(bigDecimal: BigDecimal): JsonBean =
        JNumber(bigDecimal)

      override def fromInt(int: Int): JsonBean =
        JNumber(int)

      override def fromBoolean(boolean: Boolean): JsonBean =
        JBoolean(boolean)

      override def arr(json: JsonBean*): JsonBean =
        JsonBean.fromValues(json)

      override def obj(field: (String, JsonBean)*): JsonBean =
        JsonBean.fromAttributes(field)

      override def fromValues(values: Iterable[JsonBean]): JsonBean =
        JsonBean.fromValues(values)

      override def fromFields(fields: Iterable[(String, JsonBean)]): JsonBean =
        JsonBean.fromAttributes(fields)

      final val Null: JsonBean = JNull

      override def asObject(json: JsonBean): Option[Map[String, JsonBean]] =
        json match {
          case JObject(underlying) => Some(underlying)
          case _                   => None
        }

      override def asArray(json: JsonBean): Option[Vector[JsonBean]] =
        json match {
          case JArray(values) => Some(values)
          case _              => None
        }

      override def asString(json: JsonBean): Option[String] = json match {
        case JString(value) => Some(value)
        case _              => None
      }

      override def asBoolean(json: JsonBean): Option[Boolean] = json match {
        case JBoolean(value) => Some(value)
        case _               => None
      }

      override def asNumber(json: JsonBean): Option[BigDecimal] = json match {
        case JNumber(value) => Some(value)
        case _              => None
      }

      override def asNull(json: JsonBean): Option[Unit] = json match {
        case JNull => Some(())
        case _     => None
      }

      override def isObject(json: JsonBean): Boolean = json match {
        case JObject(_) => true
        case _          => false
      }

      override def isArray(json: JsonBean): Boolean = json match {
        case JArray(_) => true
        case _         => false
      }

      override def isString(json: JsonBean): Boolean = json match {
        case JString(_) => true
        case _          => false
      }

      override def isBoolean(json: JsonBean): Boolean = json match {
        case JBoolean(_) => true
        case _           => false
      }

      override def isNumber(json: JsonBean): Boolean = json match {
        case JNumber(_) => true
        case _          => false
      }

      override def isNull(json: JsonBean): Boolean = json match {
        case JNull => true
        case _     => false
      }

      override def fold[B](json: JsonBean)(
          ifNull: => B,
          jsonBoolean: Boolean => B,
          jsonNumber: BigDecimal => B,
          jsonString: String => B,
          jsonArray: Vector[JsonBean] => B,
          jsonObject: Map[String, JsonBean] => B
      ): B =
        json match {
          case JObject(underlying) => jsonObject(underlying)
          case JArray(values)      => jsonArray(values)
          case JBoolean(value)     => jsonBoolean(value)
          case JNumber(value)      => jsonNumber(value)
          case JString(value)      => jsonString(value)
          case JNull               => ifNull
        }

      override def fromBigInt(bigInt: BigInt): JsonBean =
        JNumber(BigDecimal(bigInt))

      override def fromLong(long: Long): JsonBean =
        JNumber(BigDecimal(long))

      override def fromFloat(float: Float): Option[JsonBean] =
        Some(JNumber(BigDecimal(float.toDouble)))

      override def fromDouble(double: Double): Option[JsonBean] =
        Some(JNumber(BigDecimal(double)))

      override def mapObject(json: JsonBean)(
          f: Map[String, JsonBean] => Map[String, JsonBean]
      ): JsonBean =
        asObject(json).map(f).map(fromFields).getOrElse(json)

      override def mapArray(json: JsonBean)(
          f: Vector[JsonBean] => Vector[JsonBean]
      ): JsonBean =
        asArray(json).map(f).map(fromValues).getOrElse(json)

      override def mapString(json: JsonBean)(f: String => String): JsonBean =
        asString(json).map(f).map(string).getOrElse(json)

      override def mapBoolean(json: JsonBean)(f: Boolean => Boolean): JsonBean =
        asBoolean(json).map(f).map(boolean).getOrElse(json)

      override def mapNumber(json: JsonBean)(
          f: BigDecimal => BigDecimal
      ): JsonBean =
        asNumber(json).map(f).map(fromBigDecimal).getOrElse(json)
    }

  case object JsonBeanEvaluator extends JsonPathEvaluator[JsonBean]

  sealed trait JsonValueWrapper {

    def value: JsonBean
  }

  private case class JsonValueWrapperImpl(field: JsonBean)
      extends JsonValueWrapper {
    override def value: JsonBean = field
  }

  implicit def toJsFieldJsonBeanWrapper[T](field: T)(implicit
      w: JsonValueMagnet[T]
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
