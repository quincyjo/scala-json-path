package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.parser.{ParseError, Parsed}
import com.quincyjo.jsonpath.parser

package object literal {

  implicit final class JsonPathStringContext(sc: StringContext) {

    def jsonPath(args: Any*): JsonPath =
      parser.parse(sc.s(args: _*)).get
  }
}
