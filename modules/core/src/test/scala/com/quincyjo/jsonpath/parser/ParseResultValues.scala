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

package com.quincyjo.jsonpath.parser

import com.quincyjo.jsonpath.parser.models._
import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException}

trait ParseResultValues {

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

    def failed: ParseError =
      parseResult match {
        case Parsed(_) =>
          throw new TestFailedException(
            (_: StackDepthException) =>
              Some(
                s"The ParseResult on which failed was evoked was not a failure but a $parseResult"
              ),
            None,
            pos
          )
        case error: ParseError => error
      }
  }
}

object ParseResultValues extends ParseResultValues
