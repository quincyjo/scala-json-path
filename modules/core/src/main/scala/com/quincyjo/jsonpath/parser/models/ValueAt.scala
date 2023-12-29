package com.quincyjo.jsonpath.parser.models

final case class ValueAt[+T](value: T, index: Int, raw: String)
