package com.quincyjo.jsonpath.parser

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.parser.JsonPathParser.{
  JsonPathParserOptions,
  Token
}

import scala.collection.mutable

class JsonPathReader(parser: JsonPathParser) {

  /** Parse the input string into a [[JsonPath]] if valid, or a [[ParseError]]
    * if not.
    * @return
    *   A [[ParseResult]] of a [[JsonPath]] from the input string.
    */
  def parseInput(): ParseResult[JsonPath] = for {
    maybeRoot <- parseRoot()
    builder = List.newBuilder[ParseResult[JsonPathNode]]
    _ = while (parser.hasNext) builder addOne parseNext()
    path <- builder.result.sequence
  } yield JsonPath(maybeRoot, path)

  def take(): ParseResult[ValueAt[JsonPath]] =
    parseRoot().map { maybeRoot =>
      val builder = List.newBuilder[JsonPathNode]
      while (parser.hasNext) {
        parseNext().map(builder.addOne)
      }
      JsonPath(maybeRoot, builder.result)
    }

  private def parseRoot(): ParseResult[Option[JsonPathRoot]] =
    OptionT(parser.peek())
      .flatMapF {
        case Token.Root | Token.Current if parser.index == 0 =>
          parser.nextToken().map(Some(_))
        case other => Parsed(None)
      }
      .semiflatMap {
        case Token.Root    => Parsed(Root)
        case Token.Current => Parsed(Current)
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            Token.Root,
            Token.Current
          )
      }
      .value

  private def parseNext(): ParseResult[JsonPathNode] =
    parser.nextToken().flatMap {
      case Token.RecursiveDescent =>
        parseRecursiveDescent()
      case Token.StartSelector | Token.DotSelector =>
        parseSelector().map(Property.apply)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          parser.index,
          parser.input,
          Token.RecursiveDescent,
          Token.StartSelector,
          Token.DotSelector
        )
    }

  private def parseUnion(first: SingleSelector): ParseResult[Union] = {

    def go(
        acc: mutable.Builder[SingleSelector, Seq[SingleSelector]]
    ): ParseResult[mutable.Builder[SingleSelector, Seq[SingleSelector]]] =
      for {
        nextSelector <- parser.nextToken().flatMap {
          case Token.ValueString =>
            parser.valueAsString.map(string => Attribute(string.value))
          case Token.ValueInt =>
            parser.valueAsNumber.map(number => Index(number.value))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              Token.ValueString,
              Token.ValueInt
            )
        }
        result <- parser.nextToken().flatMap {
          case Token.EndSelector => Parsed(acc.addOne(nextSelector))
          case Token.Union       => go(acc.addOne(nextSelector))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              Token.Union,
              Token.EndSelector
            )
        }
      } yield result

    go(Seq.newBuilder[SingleSelector]).map(_.result).flatMap {
      case second :: tail => Parsed(Union(first, second, tail))
      case _ =>
        ParseError(
          s"Trailing '${Token.Union}' token at index ${parser.index}.",
          parser.index,
          parser.input
        )
    }
  }

  private def parseSlice(start: Option[Int]): ParseResult[Slice] = {

    def go(
        builder: mutable.Builder[Option[Int], Seq[Option[Int]]]
    ): ParseResult[mutable.Builder[Option[Int], Seq[Option[Int]]]] =
      parser.nextToken().flatMap {
        case Token.EndSelector => Parsed(builder)
        case Token.Slice | Token.ValueInt if builder.knownSize > 3 =>
          ParseError(s"Too many slice arguments.", parser.index, parser.input)
        case Token.Slice => go(builder.addOne(None))
        case Token.ValueInt =>
          (parser.valueAsNumber.map(_.value), parser.nextToken()).mapN {
            case (value, token) =>
              builder.addOne(Some(value))
              token match {
                case Token.EndSelector => Parsed(builder)
                case Token.Slice       => go(builder)
                case invalidToken =>
                  ParseError.invalidToken(
                    invalidToken,
                    parser.index,
                    parser.input,
                    Token.ValueInt,
                    Token.Slice,
                    Token.EndSelector
                  )
              }
          }.flatten
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            Token.ValueInt,
            Token.Slice,
            Token.EndSelector
          )
      }

    go(Seq.newBuilder[Option[Int]].addOne(start)).map(_.result).flatMap {
      parts =>
        Slice(
          parts.headOption.flatten,
          parts.lift(1).flatten,
          parts.lift(2).flatten
        )
          .map(Parsed.apply)
          .getOrElse(
            ParseError(
              "At least one slice parameter is required.",
              parser.index,
              parser.input
            )
          )
    }
  }

  def parseSlice(start: Int): ParseResult[Slice] = parseSlice(Some(start))

  def parseRecursiveDescent(): ParseResult[RecursiveDescent] =
    parser
      .currentToken()
      .fold[ParseResult[RecursiveDescent]](
        ParseError("Unexpected end of input.", parser.index, parser.input)
      ) {
        case Token.RecursiveDescent =>
          parser.peek().flatMap { nextToken =>
            nextToken
              .fold[ParseResult[RecursiveDescent]](Parsed(RecursiveDescent())) {
                case Token.Wildcard | Token.ValueInt | Token.ValueString =>
                  parseDotSelectorChild().map(RecursiveDescent.apply)
                case Token.StartSelector =>
                  parseSelector().map(RecursiveDescent.apply)
                case token => Parsed(RecursiveDescent())
              }
          }
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            Token.RecursiveDescent
          )
      }

  def parseSelector(): ParseResult[Selector] =
    parser
      .currentToken()
      .fold[ParseResult[Selector]](
        ParseError("Unexpected end of input.", parser.index, parser.input)
      ) {
        case Token.StartSelector => parseBracketSelector()
        case Token.DotSelector   => parseDotSelectorChild()
        case invalidToken =>
          ParseError
            .invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              Token.DotSelector,
              Token.StartSelector
            )
      }

  private def parseBracketSelector(): ParseResult[Selector] =
    for {
      firstToken <- parser.nextToken()
      maybeFirstSelector <- Option(firstToken).collect {
        case Token.ValueString =>
          parser.valueAsString.map(v => Attribute(v.value))
        case Token.ValueInt =>
          parser.valueAsNumber.map(v => Index(v.value))
        case Token.Wildcard => Parsed(Wildcard)
        case Token.StartExpression =>
          parser.valueAsExpression.map(v => ScriptExpression(v.value))
        case Token.StartFilterExpression =>
          parser.valueAsExpression.map(v => FilterExpression(v.value))
      }.sequence
      secondToken <- OptionT
        .fromOption[ParseResult](maybeFirstSelector)
        .semiflatMap(_ => parser.nextToken())
        .value
      selector <- (maybeFirstSelector, firstToken, secondToken) match {
        case (Some(firstSelector), _, Some(Token.EndSelector)) =>
          Parsed(firstSelector)
        case (Some(singleSelector: SingleSelector), _, Some(Token.Union)) =>
          parseUnion(singleSelector)
        case (Some(Index(index)), _, Some(Token.Slice)) =>
          parseSlice(index)
        case (None, Token.Slice, None) =>
          parseSlice(None)
        case (_, invalidToken, _) =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            Token.EndSelector,
            Token.Union,
            Token.Slice
          )
      }
    } yield selector

  private def parseDotSelectorChild(): ParseResult[Selector] =
    parser.nextToken().flatMap {
      case Token.Wildcard => Parsed(Wildcard)
      case Token.ValueString =>
        parser.valueAsString.map(_.value).map(Attribute.apply)
      case Token.ValueInt =>
        parser.valueAsNumber.map(_.value).map(Index.apply)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          parser.index,
          parser.input,
          Token.Wildcard,
          Token.ValueString,
          Token.ValueInt
        )
    }
}

object JsonPathReader {

  def apply(input: String): JsonPathReader =
    new JsonPathReader(JsonPathParser(input))

  def apply(input: String, options: JsonPathParserOptions): JsonPathReader =
    new JsonPathReader(JsonPathParser(input, options))
}
