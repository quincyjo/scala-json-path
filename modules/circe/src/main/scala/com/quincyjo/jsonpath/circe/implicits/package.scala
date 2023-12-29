package com.quincyjo.jsonpath.circe

import com.quincyjo.jsonpath.JsonSupport
import io.circe.Json

package object implicits {

  implicit val circeSupport: JsonSupport[Json] = CirceSupport
}
