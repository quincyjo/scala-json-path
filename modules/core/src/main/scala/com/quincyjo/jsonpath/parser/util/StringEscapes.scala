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

package com.quincyjo.jsonpath.parser.util

import com.quincyjo.jsonpath.parser.models.ValueAt

import java.lang
import scala.util.control.NoStackTrace

/** Handles decoding strings according to RFC 9535. This includes support for
  * escaped sequences for `\b`, `\t`, `\n`, `\f`, `\r`, `\`, `/`, `'`, `"`, and
  * `uXXXX`.
  * @see
  *   [[https://datatracker.ietf.org/doc/rfc9535]] section 2.3.1.1.
  */
object StringEscapes {

  final case class InvalidStringEncoding(
      message: String,
      string: String,
      index: Int,
      cause: Option[Throwable]
  ) extends Exception(message, cause.orNull)
      with NoStackTrace

  object InvalidStringEncoding {

    def apply(
        string: String,
        index: Int,
        cause: Option[Throwable] = None
    ): InvalidStringEncoding =
      new InvalidStringEncoding(
        s"Invalid escape in string '$string' at index $index",
        string,
        index,
        cause
      )
  }

  /** Processes the given string to replace escape sequences with their
    * corresponding values. If an invalid escape sequence is encountered, then
    * an [[InvalidStringEncoding]] is returned.
    * @param str
    *   The string to process.
    * @return
    *   A [[com.quincyjo.jsonpath.parser.models.ValueAt]] of the processed or a
    *   [[InvalidStringEncoding]].
    */
  def processEscapes(
      str: String
  ): Either[InvalidStringEncoding, ValueAt[String]] =
    str.indexOf('\\') match {
      case -1 => Right(ValueAt(str, 0, str))
      case i =>
        replace(str, i).map { case (value, i) =>
          ValueAt(value, 0, str.take(i))
        }
    }

  /** Processes escapes across the given string, returning a substring of a
    * string that is quoted starting at index 0. If the first character in the
    * given string is not a quote ('\' or '"'), then an empty
    * [[com.quincyjo.jsonpath.parser.models.ValueAt]] is returned.
    * @param str
    *   The string to process.
    * @return
    *   A [[com.quincyjo.jsonpath.parser.models.ValueAt]] of the processed or a
    *   [[InvalidStringEncoding]].
    */
  def takeQuotedString(
      str: String
  ): Either[InvalidStringEncoding, ValueAt[String]] =
    str.headOption
      .filter(c => c == '\'' || c == '"')
      .fold[Either[InvalidStringEncoding, ValueAt[String]]](
        Right(ValueAt("", 0, str))
      ) { quote =>
        str.indexWhere(c => c == quote || c == '\\', 1) match {
          case -1 => Left(InvalidStringEncoding(str, str.length - 1))
          case i =>
            replace(str, i, Some(quote)).map { case (x, i) =>
              ValueAt(x.substring(1, x.length - 1), 0, str.take(i))
            }
        }
      }

  /** Escapes any control characters in the given string assuming that it will
    * be represented as a single-quoted string.
    * @param string
    *   The string to escape.
    * @return
    *   The escaped string
    */
  def escape(string: String): String = {
    val builder = new StringBuilder()

    for (char <- string) encodeChar(char)

    @annotation.nowarn
    def encodeChar(char: Char): Unit = char match {
      case '\\' => builder.addOne('\\').addOne('\\')
      // Escapable but not required.
      // case '/'  => builder.addOne('\\').addOne('/')
      case '\b' => builder.addOne('\\').addOne('b')
      case '\t' => builder.addOne('\\').addOne('t')
      case '\n' => builder.addOne('\\').addOne('n')
      case '\f' => builder.addOne('\\').addOne('f')
      case '\r' => builder.addOne('\\').addOne('r')
      case '\'' => builder.addOne('\\').addOne('\'')
      case c if c.isSurrogate =>
        builder.addAll(f"u${c.asInstanceOf[Int]}%04x".toUpperCase)
      case c => builder.addOne(c)
    }

    builder.result()
  }

  private def replace(
      str: String,
      first: Int,
      endOn: Option[Char] = None
  ): Either[InvalidStringEncoding, (String, Int)] = {
    val findNext = endOn.fold((_: String).indexOf('\\', _: Int)) {
      endOn => (string: String, i: Int) =>
        string.indexWhere(c => c == '\\' || c == endOn, i)
    }
    val len = str.length()
    val builder = new lang.StringBuilder()

    @annotation.tailrec
    def loop(
        i: Int,
        next: Int
    ): Either[InvalidStringEncoding, (String, Int)] = {
      if (next < 0) {
        if (endOn.isEmpty) { // No more escapes, append the rest.
          if (i < len) {
            builder.append(str, i, len)
            ()
          }
          Right(builder.toString -> len)
        } else { // Trailing ending
          Left(InvalidStringEncoding(s"Unclosed quotation.", str, 0, None))
        }
      } else {
        // Append the gap between the current builder and the next escape, if any.
        if (next > i) {
          builder.append(str, i, next)
          ()
        }
        if (endOn.contains(str(next))) {
          builder.append(str(next))
          Right(builder.toString -> (next + 1))
        } else {
          var idx = next + 1
          // If trailing escape then it is invalid.
          if (idx >= len) Left(InvalidStringEncoding(str, next))
          else {
            handleEscape(str, idx, next) match {
              case left: Left[_, _] =>
                left.asInstanceOf[Either[InvalidStringEncoding, (String, Int)]]
              case Right((ch, advance)) =>
                idx += advance
                builder append ch
                loop(idx, findNext(str, idx))
            }
          }
        }
      }
    }

    loop(0, first)
  }

  private def handleEscape(
      str: String,
      index: Int,
      escapeIndex: Int
  ): Either[InvalidStringEncoding, (Char, Int)] =
    for {
      c <- Option
        .when(index < str.length)(str(index))
        .collect {
          case 'u'  => 'u'
          case 'b'  => '\b'
          case 't'  => '\t'
          case 'n'  => '\n'
          case 'f'  => '\f'
          case 'r'  => '\r'
          case '"'  => '"'
          case '\'' => '\''
          case '\\' => '\\'
          case '/'  => '/'
        }
        .toRight(InvalidStringEncoding(str, escapeIndex))
      chAndAdvance <- Option
        .unless(c == 'u')(c -> 1)
        .fold(readUEscape(str, index))(Right.apply)
    } yield chAndAdvance

  private def readUEscape(
      src: String,
      startindex: Int
  ): Either[InvalidStringEncoding, (Char, Int)] = {
    val len = src.length()

    @annotation.tailrec
    def loop(uindex: Int): Either[InvalidStringEncoding, (Char, Int)] = {

      @annotation.tailrec
      def loopCP(
          dindex: Int,
          codepoint: Int
      ): Either[InvalidStringEncoding, (Char, Int)] = {
        //supports BMP + surrogate escapes
        //but only in four hex-digit code units (uxxxx)
        if (dindex >= 4) {
          val usRead = uindex - startindex
          val digitsRead = dindex
          Right((codepoint.asInstanceOf[Char], usRead + digitsRead))
        } else if (dindex + uindex >= len)
          Left(InvalidStringEncoding(src, uindex + dindex))
        else {
          val ch = src(dindex + uindex)
          val e = ch.asDigit
          if (e >= 0 && e <= 15) loopCP(dindex + 1, (codepoint << 4) + e)
          else Left(InvalidStringEncoding(src, uindex + dindex))
        }
      }

      if (uindex >= len)
        Left(InvalidStringEncoding(src, uindex - 1))
      //allow one or more `u` characters between the
      //backslash and the code unit
      else if (src(uindex) == 'u') loop(uindex + 1)
      else loopCP(0, 0)
    }
    loop(startindex)
  }
}
