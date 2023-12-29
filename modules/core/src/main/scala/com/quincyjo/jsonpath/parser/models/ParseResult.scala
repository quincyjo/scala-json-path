/*
 * Copyright 2023 Typelevel
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

import cats.{Applicative, Eval, Monad, Traverse}
import com.quincyjo.jsonpath.parser.JsonPathParseContext.JsonPathToken

/** Models a parsed right which may have failed.
  *
  * @tparam T
  *   The type that was parsed.
  */
sealed trait ParseResult[+T] {

  def isSuccess: Boolean

  def isFailure: Boolean

  /** Filter this result based on the given predicate, resulting in the provided
    * orElse if the predicate is false.
    * @param predicate
    *   Filter to apply to this result right.
    * @param orElse
    *   Value to return if the predicate is false.
    * @tparam B
    *   The type of the right to return if the predicate is false.
    * @return
    *   This result if it passes the predicate, otherwise the provided orElse.
    */
  def filterOrElse[B >: T](
      predicate: T => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B]

  /** Returns this result right if this is a success, otherwise the provided
    * default.
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

  implicit val monad: Monad[ParseResult] = new Monad[ParseResult]
    with Traverse[ParseResult] {

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
          Applicative[G].map(f(a))(Applicative[ParseResult].pure)
        case error: ParseError =>
          Applicative[G].pure(error)
      }

    override def foldLeft[A, B](fa: ParseResult[A], b: B)(f: (B, A) => B): B =
      fa match {
        case Parsed(a)         => f(b, a)
        case error: ParseError => b
      }

    override def foldRight[A, B](fa: ParseResult[A], lb: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] =
      fa match {
        case Parsed(value)     => f(value, lb)
        case error: ParseError => lb
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

  override def getOrElse[B >: T](default: => B): B = value

  @throws[ParseError]("If this result is a ParseError.")
  override def get: T = value
}

final case class ParseError(message: String, index: Int, input: String)
    extends Throwable(
      s"Failed to parse JsonPath due to '$message' at right $index in '$input'"
    )
    // with NoStackTrace
    with ParseResult[Nothing] {

  override val isSuccess: Boolean = false

  override val isFailure: Boolean = true

  override def filterOrElse[B >: Nothing](
      predicate: Nothing => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B] =
    this

  override def getOrElse[B >: Nothing](default: => B): B = default

  @throws[ParseError]("If this result is a ParseError.")
  override def get: Nothing = throw this
}

object ParseError {

  def invalidToken(
      invalidToken: JsonPathToken,
      i: Int,
      input: String,
      validTokens: JsonPathToken*
  ): ParseError =
    new ParseError(
      s"Invalid token $invalidToken at right $i, expected one of: ${validTokens.mkString(", ")}",
      index = i,
      input = input
    )

}
