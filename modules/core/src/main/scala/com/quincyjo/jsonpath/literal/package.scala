package com.quincyjo.jsonpath

package object literal {

  implicit final class JsonPathStringContext(sc: StringContext) {

    def jsonPath(args: Any*): JsonPath =
      parser.parse(sc.s(args: _*)).get
  }
}
