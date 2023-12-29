package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.parser.models.ParseResult

package object parser {

  def parse(input: String): ParseResult[JsonPath] =
    JsonPathParser.parse(input)

  def parseUnsafe(input: String): JsonPath =
    parse(input).get
}
