package com.quincyjo.jsonpath.parser

import cats.{Applicative, Eval, Monad, Traverse}
import cats.data.OptionT
import cats.implicits.*
import com.quincyjo.jsonpath.parser.JsonPathParser.*
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath.*
import scala.annotation.tailrec
import scala.collection.mutable

class JsonPathParser(
  input: String,
  options: JsonPathParserOptions = JsonPathParserOptions.default
) {

  private var currentIndex: Int = 0
  private var currentTokenResult: ParseResult[Token] = _
  private var currentValue: Option[ValueAt[_]] = None

  def index: Int = currentIndex

  def currentToken(): Option[Token] =
    Option(currentTokenResult).flatMap {
      case Parsed(value)     => Some(value)
      case error: ParseError => None
    }

  def nextToken(): ParseResult[Token] =
    nextStep.flatMap { step =>
      currentIndex = currentIndex + step
      currentTokenResult = tokenAt(index)
      currentValue = None
      currentTokenResult
    }

  private def nextStep: ParseResult[Int] = {
    currentValue.map(v => Parsed(v.raw.length)) orElse
      Option(currentTokenResult)
        .map {
          _.flatMap {
            case token: SymbolToken => Parsed(token.length)
            case valueToken: ValueToken =>
              valueToken match {
                case Token.ValueInt    => valueAsNumber.map(_.raw.length)
                case Token.ValueString => valueAsString.map(_.raw.length)
                case Token.StartExpression | Token.StartFilterExpression =>
                  valueAsExpression.map(_.raw.length)
              }
          }
        } getOrElse Parsed(0)
  }

  def peek(): ParseResult[Option[Token]] =
    OptionT
      .whenF(hasNext)(nextStep)
      .semiflatMap(step => tokenAt(currentIndex + step))
      .value

  def hasNext: Boolean =
    currentIndex < input.length && nextStep
      .map(step => currentIndex + step < input.length)
      .getOrElse(false)

  private val attributeNameRegex = """[a-zA-Z][^,:\].\s]*""".r

  def valueAsString: ParseResult[ValueAt[String]] =
    currentTokenResult.flatMap {
      case Token.ValueString =>
        val maybeValue = input.charAt(currentIndex) match {
          case quote @ ('\'' | '"') =>
            @tailrec
            def go(i: Int, rawBuilder: StringBuilder, valueBuilder: StringBuilder): (StringBuilder, StringBuilder) = {
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
              currentIndex + 1,
              new StringBuilder().addOne(quote),
              new StringBuilder()
            ) match {
              case (raw, value) =>
                Option.when(raw.length > 1 && raw.endsWith(quote.toString))(
                  ValueAt(
                    value.result(),
                    currentIndex,
                    raw.result()
                  )
                )
            }
          case other =>
            val source = input
              .substring(currentIndex)
              .takeWhile(c => c.isLetterOrDigit || c == '_')
            Some(ValueAt(source, currentIndex, source))
        }
        maybeValue.foreach(v => currentValue = Some(v))
        maybeValue
          .map(Parsed(_))
          .getOrElse(
            ParseError(s"Hanging string value starting at index $currentIndex.", currentIndex, input)
          )
      case invalidToken =>
        ParseError.invalidToken(invalidToken, currentIndex, input, Token.ValueString)
    }

  def valueAsNumber: ParseResult[ValueAt[Int]] =
    currentTokenResult.flatMap {
      case Token.ValueInt =>
        val source = input.charAt(currentIndex) match {
          case '-' =>
            input
              .substring(currentIndex + 1)
              .takeWhile(_.isDigit)
              .prepended('-')
          case _ => input.substring(currentIndex).takeWhile(_.isDigit)
        }
        source.toIntOption
          .map { int =>
            ValueAt(int, currentIndex, source)
          }
          .map { v =>
            currentValue = Some(v)
            Parsed(v)
          }
          .getOrElse(
            ParseError(
              s"Invalid number '$source' starting at index $currentIndex.",
              currentIndex,
              input
            )
          )
      case invalidToken =>
        ParseError.invalidToken(invalidToken, currentIndex, input, Token.ValueInt)
    }

  def valueAsExpression: ParseResult[ValueAt[Expression]] =
    currentTokenResult.flatMap {
      case Token.StartExpression =>
        options.expressionParser.getValueAsExpression(input, currentIndex)
      case Token.StartFilterExpression =>
        options.expressionParser
          .getValueAsExpression(input, currentIndex + 1)
          .map { case ValueAt(expression, _, raw) =>
            ValueAt(expression, index, s"?$raw")
          }
      case invalidToken =>
        ParseError.invalidToken(invalidToken, currentIndex, input, Token.ValueString)
    }

  private def tokenAt(i: Int): ParseResult[Token] =
    input.charAt(i) match {
      case '$' => Parsed(Token.Root)
      case '@' => Parsed(Token.Current)
      case '*' => Parsed(Token.Wildcard)
      case '[' => Parsed(Token.StartSelector)
      case ']' => Parsed(Token.EndSelector)
      case '.' =>
        if (input.lift(i + 1).contains('.')) Parsed(Token.RecursiveDescent)
        else Parsed(Token.DotSelector)
      case '?' if input.lift(i + 1).contains('(') =>
        Parsed(Token.StartFilterExpression)
      case '(' => Parsed(Token.StartExpression)
      // case ')'                                    => Parsed(Token.EndExpression)
      case ','             => Parsed(Token.Union)
      case ':'             => Parsed(Token.Slice)
      case '\'' | '"'      => Parsed(Token.ValueString)
      case c if c.isLetter => Parsed(Token.ValueString)
      case '-'             => Parsed(Token.ValueInt)
      case c if c.isDigit  => Parsed(Token.ValueInt)
      case c =>
        ParseError(
          s"Invalid character '$c' at index $i in '$input'.",
          i,
          input
        )
    }
}

object JsonPathParser {

  final case class JsonPathParserOptions(
    expressionParser: ExpressionParser[JsonPath.Expression] = ExpressionParser.BalancedExpressionParser
  )

  object JsonPathParserOptions {

    final val default = JsonPathParserOptions()
  }

  final case class ValueAt[+T](value: T, index: Int, raw: String)

  sealed abstract class Token

  sealed abstract class SymbolToken(value: String) extends Token {

    val length: Int = value.length
  }

  sealed trait ValueToken extends Token

  object Token {

    case object Root extends SymbolToken("$")
    case object Current extends SymbolToken("@")
    case object Wildcard extends SymbolToken("*")
    case object StartSelector extends SymbolToken("[")
    case object EndSelector extends SymbolToken("]")
    case object DotSelector extends SymbolToken(".")
    case object RecursiveDescent extends SymbolToken("..")
    case object Union extends SymbolToken(",")
    case object Slice extends SymbolToken(":")
    case object StartFilterExpression extends ValueToken // SymbolToken("?(")
    case object StartExpression extends ValueToken // SymbolToken("(")
    // final case object EndExpression extends SymbolToken(")")

    case object ValueString extends ValueToken
    case object ValueInt extends ValueToken
  }

}
