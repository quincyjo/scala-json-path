package com.quincyjo.jsonpath.parser

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.parser.JsonPathParser.ValueAt
import com.quincyjo.jsonpath.parser.ExpressionParserNew.ExpressionToken._

import scala.annotation.tailrec
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps

object ExpressionParserNew {

  def parse(string: String): ParseContext =
    ExpressionParserNew.ParseContext(string)

  final case class ParseContext(
      input: String,
      index: Int = 0,
      currentTokenResult: OptionT[ParseResult, ExpressionToken] =
        OptionT.none[ParseResult, ExpressionToken]
  ) {
    private var currentValue: Option[ValueAt[_]] = None

    def currentToken: Option[ExpressionToken] =
      currentTokenResult.value.getOrElse(None)

    def nextToken(): ParseContext = {
      val newIndex = nextIndex
      ParseContext(
        input,
        newIndex.getOrElse(index),
        currentTokenResult = OptionT.liftF(newIndex.flatMap(tokenAt))
      )
    }

    def hasNext: Boolean =
      currentTokenResult.value.isSuccess &&
        nextIndex.map(_ < input.length).getOrElse(false)

    def peek: ParseResult[Option[ExpressionToken]] =
      OptionT
        .whenF(hasNext)(nextIndex)
        .semiflatMap(tokenAt)
        .value

    def tokenAt(i: Int): ParseResult[ExpressionToken] =
      OptionT
        .fromOption[ParseResult](input.lift(i))
        .orElseF(ParseError("Unexpected end of input.", i, input))
        .collect {
          case '!' =>
            if (input.lift(i + 1).contains('=')) NotEqual
            else Not
          case '<' =>
            if (input.lift(i + 1).contains('=')) LessThanOrEqualTo
            else LessThan
          case '>' =>
            if (input.lift(i + 1).contains('=')) GreaterThanOrEqualTo
            else GreaterThan
          case '=' if input.lift(i + 1).contains('=') =>
            Equal
          case '&' if input.lift(i + 1).contains('&') =>
            And
          case '|' if input.lift(i + 1).contains('|') =>
            Or
          case '$'            => Root
          case '@'            => Current
          case '('            => OpenParenthesis
          case ')'            => CloseParenthesis
          case '+'            => Plus
          case '-'            => Minus
          case '*'            => Multiply
          case '/'            => Divide
          case '\'' | '"'     => ValueString
          case c if c.isDigit => ValueNumber
          case 't' | 'f'      => ValueBoolean
        }
        .getOrElseF(
          ParseError(s"Unexpected character '${input(i)}'", i, input)
        )

    def nextIndex: ParseResult[Int] =
      OptionT
        .fromOption[ParseResult](currentValue)
        .map(v => input.indexWhere(!_.isWhitespace, index + v.raw.length))
        .orElse(
          currentTokenResult
            .semiflatMap {
              case ValueBoolean =>
                valueAsBoolean.map { value =>
                  input.indexWhere(!_.isWhitespace, index + value.raw.length)
                }
              case ValueString =>
                valueAsString.map { value =>
                  input.indexWhere(!_.isWhitespace, index + value.raw.length)
                }
              case ValueNumber =>
                valueAsNumber.map { value =>
                  input.indexWhere(!_.isWhitespace, index + value.raw.length)
                }
              case Root | Current =>
                valueAsJsonPath.map { value =>
                  input.indexWhere(!_.isWhitespace, index + value.raw.length)
                }
              case token: SymbolToken =>
                Parsed(input.indexWhere(!_.isWhitespace, index + token.length))
            }
        )
        .semiflatMap {
          case n if n == -1 || n >= input.length =>
            ParseError("Unexpected end of input.", index, input)
          case i => Parsed(i)
        }
        .getOrElse(0)

    protected def valueAs[T](
        pf: PartialFunction[ExpressionToken, ParseResult[ValueAt[T]]]
    ): ParseResult[ValueAt[T]] =
      currentTokenResult
        .semiflatMap { token =>
          pf.lift(token)
            .getOrElse(ParseError(s"Unexpected token $token", index, input))
        }
        .getOrElseF(
          ParseError(
            s"Unexpected end of input.",
            index,
            input
          )
        )
        .tap(_.map(v => currentValue = Some(v)))

    def valueAsBoolean: ParseResult[ValueAt[Boolean]] =
      valueAs[Boolean] { case ValueBoolean =>
        if (input.startsWith("true")) Parsed(ValueAt(true, 0, "true"))
        else if (input.startsWith("false")) Parsed(ValueAt(false, 0, "false"))
        else
          ParseError(
            s"Expected boolean value but was '${input.substring(index).takeWhile(_.isLetter)}'",
            index,
            input
          )
      }

    def valueAsString: ParseResult[ValueAt[String]] =
      valueAs { case ValueString =>
        OptionT
          .fromOption[ParseResult](input.lift(index))
          .semiflatMap {
            case quote @ ('\'' | '"') =>
              @tailrec
              def go(
                  i: Int,
                  rawBuilder: StringBuilder,
                  valueBuilder: StringBuilder
              ): (StringBuilder, StringBuilder) = {
                if (i < input.length) {
                  val c = input.charAt(i)
                  if (c == quote && !rawBuilder.lastOption.contains('\\'))
                    (rawBuilder.addOne(quote), valueBuilder)
                  else if (c == quote) {
                    valueBuilder.update(valueBuilder.length() - 1, quote)
                    go(i + 1, rawBuilder.addOne(quote), valueBuilder)
                  } else
                    go(i + 1, rawBuilder.addOne(c), valueBuilder.addOne(c))
                } else (rawBuilder, valueBuilder)
              }

              go(
                index + 1,
                new StringBuilder().addOne(quote),
                new StringBuilder()
              ) match {
                case (raw, value) =>
                  if (raw.length > 1 && raw.endsWith(quote.toString))
                    Parsed(ValueAt(value.result(), index, raw.result()))
                  else ParseError("Unclosed quotation.", index, input)
              }

            case invalidChar =>
              ParseError(
                s"Expected JSON string but was '$invalidChar'",
                index,
                input
              )
          }
          .getOrElseF(ParseError("Unexpected end of input.", index, input))
      }

    def valueAsNumber: ParseResult[ValueAt[BigDecimal]] =
      valueAs[BigDecimal] { case ValueNumber =>
        var hasReadDecimal = false
        val end = Option(
          input.indexWhere(
            c =>
              !c.isDigit || c == '.' && {
                (hasReadDecimal).tap(_ => hasReadDecimal = true)
              },
            index
          )
        ).filter(_ > index)
        val raw =
          end.fold(input.substring(index))(input.substring(index, _))
        Try(BigDecimal(raw)).fold(
          throwable => ParseError(throwable.getMessage, index, input),
          number => Parsed(ValueAt(number, index, raw))
        )
      }

    def valueAsJsonPath: ParseResult[ValueAt[JsonPath]] =
      valueAs[JsonPath] { case ExpressionToken.Root | ExpressionToken.Current =>
        JsonPathReader(input.substring(index))
          .take()
          .map {
            _.copy(
              index = index
            )
          }
      }
  }

  sealed trait ExpressionToken

  object ExpressionToken {

    sealed trait SymbolToken extends ExpressionToken {

      def symbol: String
      def length: Int = symbol.length
    }

    sealed trait ValueToken extends ExpressionToken

    sealed trait OperatorToken extends ExpressionToken
    sealed trait BinaryToken extends OperatorToken

    object Not extends SymbolToken with OperatorToken {
      override def symbol: String = "!"
    }

    case object And extends SymbolToken with BinaryToken {
      override def symbol: String = "&&"
    }

    case object Or extends SymbolToken with BinaryToken {
      override def symbol: String = "||"
    }

    case object Equal extends SymbolToken with BinaryToken {
      override def symbol: String = "=="
    }

    case object NotEqual extends SymbolToken with BinaryToken {
      override def symbol: String = "!="
    }

    case object LessThan extends SymbolToken with BinaryToken {
      override def symbol: String = "<"
    }

    case object LessThanOrEqualTo extends SymbolToken with BinaryToken {
      override def symbol: String = "<="
    }

    case object GreaterThan extends SymbolToken with BinaryToken {
      override def symbol: String = ">"
    }

    case object GreaterThanOrEqualTo extends SymbolToken with BinaryToken {
      override def symbol: String = ">="
    }

    case object Plus extends SymbolToken with BinaryToken {
      override def symbol: String = "+"
    }

    case object Minus extends SymbolToken with BinaryToken {
      override def symbol: String = "-"
    }

    case object Multiply extends SymbolToken with BinaryToken {
      override def symbol: String = "*"
    }

    case object Divide extends SymbolToken with BinaryToken {
      override def symbol: String = "/"
    }

    case object Root extends SymbolToken with ValueToken {
      override def symbol: String = "$"
    }

    case object Current extends SymbolToken with ValueToken {
      override def symbol: String = "@"
    }

    case object OpenParenthesis extends SymbolToken with OperatorToken {
      override def symbol: String = "("
    }
    case object CloseParenthesis extends SymbolToken {
      override def symbol: String = ")"
    }
    case object ValueString extends ValueToken
    case object ValueBoolean extends ValueToken
    case object ValueNumber extends ValueToken
  }
}
