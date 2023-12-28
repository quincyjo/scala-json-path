package com.quincyjo.jsonpath.parser.models

trait ParserToken

object ParserToken {

  trait SymbolToken extends ParserToken {

    def symbol: String

    def length: Int = symbol.length
  }

  trait ValueToken extends ParserToken
}
