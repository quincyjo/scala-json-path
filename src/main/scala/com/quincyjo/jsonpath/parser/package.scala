package com.quincyjo.jsonpath

package object parser {

  def parse(input: String): ParseResult[JsonPath] =
    JsonPathReader(input).parseInput()

  def parseUnsafe(input: String): JsonPath =
    parse(input).get
}
