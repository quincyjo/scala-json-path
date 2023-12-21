package com.quincyjo.jsonpath.parser

import cats.{Applicative, Eval, Monad, Traverse}
import com.quincyjo.jsonpath.parser.JsonPathParser.Token
import scala.util.control.NoStackTrace

sealed trait ParseResult[+T] {

  def filterOrElse[B >: T](
    predicate: T => Boolean,
    orElse: => ParseResult[B]
  ): ParseResult[B]

  def getOrElse[B >: T](default: => B): B
  
  def get: T
}

final case class Parsed[T](value: T) extends ParseResult[T] {

  override def filterOrElse[B >: T](
    predicate: T => Boolean,
    orElse: => ParseResult[B]
  ): ParseResult[B] =
    if (predicate(value)) this else orElse

  override def getOrElse[B >: T](default: => B): B = value

  override def get: T = value
}

final case class ParseError(message: String, index: Int, input: String)
    extends Throwable(s"Failed to parse JsonPath due to '$message' at index $index in '$input'")
    with NoStackTrace
    with ParseResult[Nothing] {

  override def filterOrElse[B >: Nothing](
    predicate: Nothing => Boolean,
    orElse: => ParseResult[B]
  ): ParseResult[B] =
    this

  override def getOrElse[B >: Nothing](default: => B): B = default
  
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

  implicit val monad: Monad[ParseResult] = new Monad[ParseResult] with Traverse[ParseResult] {
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
