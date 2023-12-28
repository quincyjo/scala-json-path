package com.quincyjo.jsonpath.parser

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.parser.JsonPathParser._
import com.quincyjo.jsonpath.parser.models._

import scala.annotation.tailrec

class JsonPathParser(
    val input: String,
    val options: JsonPathParserOptions = JsonPathParserOptions.default
) {

  private var currentIndex: Int = 0
  private var currentTokenResult: ParseResult[JsonPathToken] = _
  private var currentValue: Option[ValueAt[_]] = None

  def index: Int = currentIndex

  def currentToken(): Option[JsonPathToken] =
    Option(currentTokenResult).flatMap {
      case Parsed(value)     => Some(value)
      case error: ParseError => None
    }

  def nextToken(): ParseResult[JsonPathToken] =
    nextStep.flatMap { step =>
      currentIndex = currentIndex + step
      currentTokenResult = tokenAt(index)
      currentValue = None
      currentTokenResult
    }

  def nextStep: ParseResult[Int] = {
    currentValue.map(v => Parsed(v.raw.length)) orElse
      Option(currentTokenResult)
        .map {
          _.flatMap {
            case token: SymbolToken => Parsed(token.length)
            case valueToken: ValueToken =>
              valueToken match {
                case JsonPathToken.ValueInt => valueAsNumber.map(_.raw.length)
                case JsonPathToken.ValueString =>
                  valueAsString.map(_.raw.length)
                case JsonPathToken.StartExpression |
                    JsonPathToken.StartFilterExpression =>
                  valueAsExpression.map(_.raw.length)
              }
          }
        } getOrElse Parsed(0)
  }

  def peek(): ParseResult[Option[JsonPathToken]] =
    OptionT
      .whenF(hasNext)(nextStep)
      .semiflatMap(step => tokenAt(currentIndex + step))
      .value

  def hasNext: Boolean =
    Option(currentTokenResult).forall(
      _.isSuccess
    ) && currentIndex < input.length && nextStep
      .map(step => currentIndex + step < input.length)
      .getOrElse(false)

  private val attributeNameRegex = """[a-zA-Z][^,:\].\s]*""".r

  def valueAsString: ParseResult[ValueAt[String]] =
    currentTokenResult.flatMap {
      case JsonPathToken.ValueString =>
        val maybeValue = input.charAt(currentIndex) match {
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
            ParseError(
              s"Hanging string right starting at right $currentIndex.",
              currentIndex,
              input
            )
          )
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          currentIndex,
          input,
          JsonPathToken.ValueString
        )
    }

  def valueAsNumber: ParseResult[ValueAt[Int]] =
    currentTokenResult.flatMap {
      case JsonPathToken.ValueInt =>
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
              s"Invalid number '$source' starting at right $currentIndex.",
              currentIndex,
              input
            )
          )
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          currentIndex,
          input,
          JsonPathToken.ValueInt
        )
    }

  def valueAsExpression: ParseResult[ValueAt[Expression]] =
    currentTokenResult.flatMap {
      case JsonPathToken.StartExpression =>
        options.expressionParser.getValueAsExpression(input, currentIndex)
      case JsonPathToken.StartFilterExpression =>
        options.expressionParser
          .getValueAsExpression(input, currentIndex + 1)
          .map { case ValueAt(expression, _, raw) =>
            ValueAt(expression, index, s"?$raw")
          }
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          currentIndex,
          input,
          JsonPathToken.ValueString
        )
    }

  private def tokenAt(i: Int): ParseResult[JsonPathToken] =
    input.charAt(i) match {
      case '$' => Parsed(JsonPathToken.Root)
      case '@' => Parsed(JsonPathToken.Current)
      case '*' => Parsed(JsonPathToken.Wildcard)
      case '[' => Parsed(JsonPathToken.StartSelector)
      case ']' => Parsed(JsonPathToken.EndSelector)
      case '.' =>
        if (input.lift(i + 1).contains('.'))
          Parsed(JsonPathToken.RecursiveDescent)
        else Parsed(JsonPathToken.DotSelector)
      case '?' if input.lift(i + 1).contains('(') =>
        Parsed(JsonPathToken.StartFilterExpression)
      case '(' => Parsed(JsonPathToken.StartExpression)
      // case ')'                                    => Parsed(JsonPathToken.EndExpression)
      case ','             => Parsed(JsonPathToken.Union)
      case ':'             => Parsed(JsonPathToken.Slice)
      case '\'' | '"'      => Parsed(JsonPathToken.ValueString)
      case c if c.isLetter => Parsed(JsonPathToken.ValueString)
      case '-'             => Parsed(JsonPathToken.ValueInt)
      case c if c.isDigit  => Parsed(JsonPathToken.ValueInt)
      case c =>
        ParseError(
          s"Invalid character '$c' at right $i in '$input'.",
          i,
          input
        )
    }
}

object JsonPathParser {

  def apply(input: String): JsonPathParser =
    new JsonPathParser(input)

  def apply(input: String, options: JsonPathParserOptions): JsonPathParser =
    new JsonPathParser(input, options)

  final case class JsonPathParserOptions(
      expressionParser: ExpressionParser[JsonPath.Expression] =
        ExpressionParser.BalancedExpressionParser
  )

  object JsonPathParserOptions {

    final val default = JsonPathParserOptions()
  }

  sealed abstract class JsonPathToken extends ParserToken

  sealed abstract class SymbolToken(override val symbol: String)
      extends JsonPathToken
      with ParserToken.SymbolToken

  sealed trait ValueToken extends JsonPathToken with ParserToken.ValueToken

  object JsonPathToken {

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
