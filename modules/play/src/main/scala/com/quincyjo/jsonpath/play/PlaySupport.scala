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

import com.quincyjo.jsonpath.JsonSupport
import play.api.libs.json._

trait PlaySupport {

  implicit val playSupport: JsonSupport[JsValue] = PlaySupport
}

object PlaySupport extends JsonSupport[JsValue] {

  override def string(string: String): JsValue =
    JsString(string)

  override def number(bigDecimal: BigDecimal): JsValue =
    JsNumber(bigDecimal)

  override def boolean(boolean: Boolean): JsValue =
    JsBoolean(boolean)

  override def Null: JsValue =
    JsNull

  override def asObject(json: JsValue): Option[Map[String, JsValue]] =
    json match {
      case JsObject(underlying) => Option(underlying.toMap)
      case _                    => None
    }

  override def asArray(json: JsValue): Option[Vector[JsValue]] =
    json match {
      case JsArray(values) => Some(values.toVector)
      case _               => None
    }

  override def asString(json: JsValue): Option[String] =
    json match {
      case JsString(value) => Some(value)
      case _               => None
    }

  override def asBoolean(json: JsValue): Option[Boolean] =
    json match {
      case boolean: JsBoolean => Some(boolean.value)
      case _                  => None
    }

  override def asNumber(json: JsValue): Option[BigDecimal] =
    json match {
      case JsNumber(value) => Some(value)
      case _               => None
    }

  override def asNull(json: JsValue): Option[Unit] =
    json match {
      case JsNull => Some(())
      case _      => None
    }

  override def isObject(json: JsValue): Boolean =
    json match {
      case JsObject(_) => true
      case _           => false
    }

  override def isArray(json: JsValue): Boolean =
    json match {
      case JsArray(_) => true
      case _          => false
    }

  override def isString(json: JsValue): Boolean =
    json match {
      case JsString(_) => true
      case _           => false
    }

  override def isBoolean(json: JsValue): Boolean =
    json match {
      case JsBoolean(_) => true
      case _            => false
    }

  override def isNumber(json: JsValue): Boolean =
    json match {
      case JsNumber(_) => true
      case _           => false
    }

  override def isNull(json: JsValue): Boolean =
    json match {
      case JsNull => true
      case _      => false
    }

  override def fold[B](json: JsValue)(
      ifNull: => B,
      jsonBoolean: Boolean => B,
      jsonNumber: BigDecimal => B,
      jsonString: String => B,
      jsonArray: Vector[JsValue] => B,
      jsonObject: Map[String, JsValue] => B
  ): B = json match {
    case JsNull               => ifNull
    case boolean: JsBoolean   => jsonBoolean(boolean.value)
    case JsNumber(value)      => jsonNumber(value)
    case JsString(value)      => jsonString(value)
    case JsArray(value)       => jsonArray(value.toVector)
    case JsObject(underlying) => jsonObject(underlying.toMap)
  }
}
