package com.quincyjo.jsonpath.parser

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.Expression
import com.quincyjo.jsonpath.parser.JsonPathParseContext._
import com.quincyjo.jsonpath.parser.models._

final case class JsonPathParseContext private(
    input: String,
    index: Int = 0,
    currentTokenResult: OptionT[ParseResult, JsonPathToken] =
      OptionT.none[ParseResult, JsonPathToken]
) extends ParseContext[JsonPathToken] {

  override def nextToken(): JsonPathParseContext = {
    val newIndex = nextIndex
    JsonPathParseContext(
      input,
      newIndex.getOrElse(index),
      OptionT.liftF(newIndex.flatMap(tokenAt))
    )
  }

  def valueAsString: ParseResult[ValueAt[String]] =
    valueAs { case JsonPathToken.ValueString =>
      input.charAt(index) match {
        case quote @ ('\'' | '"') =>
          parseQuotedString(index)
        case other =>
          val source = input
            .substring(index)
            .takeWhile(c => c.isLetterOrDigit || c == '_')
          Parsed(ValueAt(source, index, source))
      }
    }

  def valueAsNumber: ParseResult[ValueAt[Int]] =
    valueAs { case JsonPathToken.ValueInt =>
      val source = input.charAt(index) match {
        case '-' =>
          input
            .substring(index + 1)
            .takeWhile(_.isDigit)
            .prepended('-')
        case _ => input.substring(index).takeWhile(_.isDigit)
      }
      source.toIntOption
        .map { int =>
          Parsed(ValueAt(int, index, source))
        }
        .getOrElse(
          ParseError(
            s"Invalid number '$source' starting at right $index.",
            index,
            input
          )
        )
    }

  def valueAsExpression: ParseResult[ValueAt[Expression]] =
    valueAs {
      case JsonPathToken.StartExpression =>
        val balanced = BalancedExpressionReader(input.substring(index)).takeGroup
        ExpressionParser.parse(balanced).map { expression =>
          ValueAt(expression, index, balanced)
        }
      case JsonPathToken.StartFilterExpression =>
        val balanced = BalancedExpressionReader(input.substring(index + 1)).takeGroup
        ExpressionParser.parse(balanced).map { expression =>
          ValueAt(expression, index, s"?$balanced")
        }
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          index,
          input,
          JsonPathToken.ValueString
        )
    }

  override def tokenAt(i: Int): ParseResult[JsonPathToken] =
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

  override def value(): ParseResult[ValueAt[Any]] =
    valueAs {
      case JsonPathToken.ValueInt =>
        valueAsNumber
      case JsonPathToken.ValueString =>
        valueAsString
      case JsonPathToken.StartExpression |
          JsonPathToken.StartFilterExpression =>
        valueAsExpression
    }
}

object JsonPathParseContext {

  def apply(input: String): JsonPathParseContext =
    new JsonPathParseContext(input)

  sealed abstract class JsonPathToken extends ParserToken

  sealed abstract class SymbolToken(override val symbol: String)
      extends JsonPathToken
      with ParserToken.SymbolToken

  sealed trait ValueToken
      extends JsonPathToken
      with ParserToken.ValueToken

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
