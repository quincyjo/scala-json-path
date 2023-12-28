package com.quincyjo.jsonpath.parser

import com.quincyjo.jsonpath.parser.models._
import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait ParseResultValues {

  import scala.language.implicitConversions

  implicit def convertParseResultToValuable[T](parseResult: ParseResult[T])(
      implicit pos: source.Position
  ): ParseResultValuable[T] =
    new ParseResultValuable[T](parseResult, pos)

  class ParseResultValuable[T](
      parseResult: ParseResult[T],
      pos: source.Position
  ) {

    def value: T =
      parseResult match {
        case Parsed(value) => value
        case error: ParseError =>
          throw new TestFailedException(
            (_: StackDepthException) =>
              Some(
                s"The ParseResult on which right was evoked was not a success but a $parseResult"
              ),
            Some(error),
            pos
          )
      }
  }
}

object ParseResultValues extends ParseResultValues
