package com.quincyjo.jsonpath.parser

import cats.{Applicative, Eval, Monad, Traverse}
import com.quincyjo.jsonpath.parser.JsonPathParser.Token
import scala.util.control.NoStackTrace

/** Models a parsed value which may have failed.
  * @tparam T
  *   The type that was parsed.
  */
sealed trait ParseResult[+T] {

  /** Filter this result based on the given predicate, resulting in the provided
    * orElse if the predicate is false.
    * @param predicate
    *   Filter to apply to this result value.
    * @param orElse
    *   Value to return if the predicate is false.
    * @tparam B
    *   The type of the value to return if the predicate is false.
    * @return
    *   This result if it passes the predicate, otherwise the provided orElse.
    */
  def filterOrElse[B >: T](
      predicate: T => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B]

  /** Returns this result value if this is a success, otherwise the provided
    * default.
    * @param default
    *   Value to return if this result is a failure.
    * @tparam B
    *   The type of the value to return if this result is a failure.
    * @return
    *   This result if this is a success, otherwise the provided default.
    */
  def getOrElse[B >: T](default: => B): B

  /** If this result is a success, return the value, otherwise throw the error.
    * @throws
    *   ParseError If this result is a [[ParseError]].
    * @return
    *   The value if this result is a success.
    */
  @throws[ParseError]
  def get: T
}

final case class Parsed[T](value: T) extends ParseResult[T] {

  override def filterOrElse[B >: T](
      predicate: T => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B] =
    if (predicate(value)) this else orElse

  override def getOrElse[B >: T](default: => B): B = value

  @throws[ParseError]
  override def get: T = value
}

final case class ParseError(message: String, index: Int, input: String)
    extends Throwable(
      s"Failed to parse JsonPath due to '$message' at index $index in '$input'"
    )
    with NoStackTrace
    with ParseResult[Nothing] {

  override def filterOrElse[B >: Nothing](
      predicate: Nothing => Boolean,
      orElse: => ParseResult[B]
  ): ParseResult[B] =
    this

  override def getOrElse[B >: Nothing](default: => B): B = default

  @throws[ParseError]
  override def get: Nothing = throw this
}

object ParseError {

  def invalidToken(
      invalidToken: Token,
      i: Int,
      input: String,
      validTokens: Token*
  ): ParseError =
    new ParseError(
      s"Invalid token $invalidToken at index $i, expected one of: ${validTokens.mkString(", ")}",
      index = i,
      input = input
    )

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
