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

package com.quincyjo.jsonpath.parser.models

import cats.{Applicative, Eval, Monad, MonadError, Traverse}

import scala.util.control.NoStackTrace

/** Models a parsed right which may have failed.
  *
  * @tparam T
  *   The type that was parsed.
  */
sealed trait ParseResult[+T] {

  /** True if this is a success, false if it is a failure.
    */
  def isSuccess: Boolean

  /** True if this is a failure, false if it is a success.
    */
  def isFailure: Boolean

  /** If this parse result is a success, keep it if the value passes the
    * <pre>predicate</pre>, or return the result of the <pre>orElse</pre>
    * statement if it fails the <pre>predicate</pre>. If this parse result is a
    * failure, then the failure is returned.
    *
    * This is equivalent to:
    * {{{
    * parseResult match {
    *   case Parsed(value) if predicate(value) => Parsed(value)
    *   case Parsed(_) => orElse
    *   case error: ParseError => error
    * }}}
    * This is also equivalent to:
    * {{{
    * parseResult.flatMap { value =>
    *   if (predicate(value)) Parsed(value) else orElse
    * }
    * }}}
    * @param predicate
    *   Filter to apply to this result value.
    * @param orElse
    *   Value to return if the predicate is false.
    * @tparam B
    *   The type of the right to return if the predicate is false.
    * @return
    *   If this is a failure, this failure, otherwise this result if it passes
    *   the predicate, otherwise the provided orElse.
    */
  def filterOrElse[B >: T](
      predicate: T => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B]

  /** Returns the result of applying <pre>f</pre> to this parse result value if
    * the parse result is a success. Otherwise, evaluates expression
    * <pre>orElse</pre>
    *
    * This is equivalent to:
    * {{{
    * parseResult match {
    *   case Parsed(x)     => f(x)
    *   case _: ParseError => ifEmpty
    * }
    * }}}
    * This is also equivalent to:
    * {{{
    * parseResult map f getOrElse ifEmpty
    * }}}
    * @param orElse
    *   the expression to evaluate if this result is a failure.
    * @param f
    *   the function to apply if this result is a success.
    */
  def fold[B](orElse: => B)(f: T => B): B

  /** Returns this result right if this is a success, otherwise the result of
    * <pre>default</pre>.
    *
    * This is equivalent to:
    * {{{
    * parseResult match {
    *   case Parsed(value) => value
    *   case _: ParseError => default
    * }
    * }}}
    * @param default
    *   Value to return if this result is a failure.
    * @tparam B
    *   The type of the right to return if this result is a failure.
    * @return
    *   This result if this is a success, otherwise the provided default.
    */
  def getOrElse[B >: T](default: => B): B

  /** If this result is a success, return the right, otherwise throw the error.
    * @return
    *   The right if this result is a success.
    */
  @throws[ParseError]("If this result is a ParseError.")
  def get: T
}

object ParseResult {

  implicit val monad: Monad[ParseResult] =
    new MonadError[ParseResult, ParseError] with Traverse[ParseResult] {

      override def pure[A](x: A): ParseResult[A] =
        Parsed(x)

      override def flatMap[A, B](
          fa: ParseResult[A]
      )(f: A => ParseResult[B]): ParseResult[B] =
        fa match {
          case Parsed(value)     => f(value)
          case error: ParseError => error
        }

      @annotation.tailrec
      override def tailRecM[A, B](
          a: A
      )(f: A => ParseResult[Either[A, B]]): ParseResult[B] = f(a) match {
        case failure: ParseError => failure
        case Parsed(Right(b))    => Parsed(b)
        case Parsed(Left(a))     => tailRecM(a)(f)
      }

      override def traverse[G[_]: Applicative, A, B](
          fa: ParseResult[A]
      )(f: A => G[B]): G[ParseResult[B]] =
        fa match {
          case Parsed(a) =>
            Applicative[G].map(f(a))(pure)
          case error: ParseError =>
            Applicative[G].pure(error)
        }

      override def foldLeft[A, B](fa: ParseResult[A], b: B)(f: (B, A) => B): B =
        fa match {
          case Parsed(a)     => f(b, a)
          case _: ParseError => b
        }

      override def foldRight[A, B](fa: ParseResult[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] =
        fa match {
          case Parsed(value) => f(value, lb)
          case _: ParseError => lb
        }

      override def raiseError[A](e: ParseError): ParseResult[A] = e

      override def handleErrorWith[A](fa: ParseResult[A])(
          f: ParseError => ParseResult[A]
      ): ParseResult[A] = fa match {
        case parsed: Parsed[_] => parsed.asInstanceOf[Parsed[A]]
        case error: ParseError => f(error)
      }
    }
}

final case class Parsed[T](value: T) extends ParseResult[T] {

  override val isSuccess: Boolean = true

  override val isFailure: Boolean = false

  override def filterOrElse[B >: T](
      predicate: T => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B] =
    if (predicate(value)) this else orElse

  def fold[B](orElse: => B)(f: T => B): B = f(value)

  override def getOrElse[B >: T](default: => B): B = value

  @throws[ParseError]("If this result is a ParseError.")
  override def get: T = value
}

final case class ParseError(
    message: String,
    index: Int,
    input: String,
    cause: Option[Throwable] = None
) extends Throwable(
      s"Failed to parse JsonPath at index $index in '$input': $message",
      cause.orNull
    )
    with ParseResult[Nothing]
    with NoStackTrace {

  override val isSuccess: Boolean = false

  override val isFailure: Boolean = true

  override def filterOrElse[B >: Nothing](
      predicate: Nothing => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B] = this

  def fold[B](orElse: => B)(f: Nothing => B): B = orElse

  override def getOrElse[B >: Nothing](default: => B): B = default

  @throws[ParseError]("If this result is a ParseError.")
  override def get: Nothing = throw this
}

object ParseError {

  def invalidToken[TokenType <: ParserToken](
      invalidToken: TokenType,
      i: Int,
      input: String,
      validTokens: TokenType*
  ): ParseError =
    new ParseError(
      expectedMessage(validTokens).fold(invalidTokenMessage(invalidToken, i)) {
        x =>
          s"${invalidTokenMessage(invalidToken, i)}, $x"
      },
      index = i,
      input = input
    )

  private def invalidTokenMessage(invalidToken: ParserToken, i: Int): String =
    s"Invalid token $invalidToken at index $i"

  private def expectedMessage[TokenType <: ParserToken](
      validTokens: Iterable[ParserToken]
  ): Option[String] = {
    validTokens.headOption.map { head =>
      if (validTokens.size == 1) s"expected $head"
      else s"expected one of: ${validTokens.tail.mkString(", ")}"
    }
  }
}
