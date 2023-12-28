package com.quincyjo.jsonpath.parser

import cats.data.OptionT
import cats.implicits._
import com.quincyjo.jsonpath.JsonPath
import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.parser.JsonPathParser.{
  JsonPathParserOptions,
  JsonPathToken
}
import com.quincyjo.jsonpath.parser.models._

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
      ValueAt(
        JsonPath(maybeRoot, builder.result),
        0,
        parser.input.take(parser.index + parser.nextStep.getOrElse(0))
      )
    }

  private def parseRoot(): ParseResult[Option[JsonPathRoot]] =
    OptionT(parser.peek())
      .flatMapF {
        case JsonPathToken.Root | JsonPathToken.Current if parser.index == 0 =>
          parser.nextToken().map(Some(_))
        case other => Parsed(None)
      }
      .semiflatMap {
        case JsonPathToken.Root    => Parsed(Root)
        case JsonPathToken.Current => Parsed(Current)
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            JsonPathToken.Root,
            JsonPathToken.Current
          )
      }
      .value

  private def parseNext(): ParseResult[JsonPathNode] =
    parser.nextToken().flatMap {
      case JsonPathToken.RecursiveDescent =>
        parseRecursiveDescent()
      case JsonPathToken.StartSelector | JsonPathToken.DotSelector =>
        parseSelector().map(Property.apply)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          parser.index,
          parser.input,
          JsonPathToken.RecursiveDescent,
          JsonPathToken.StartSelector,
          JsonPathToken.DotSelector
        )
    }

  private def parseUnion(first: SingleSelector): ParseResult[Union] = {

    def go(
        acc: mutable.Builder[SingleSelector, Seq[SingleSelector]]
    ): ParseResult[mutable.Builder[SingleSelector, Seq[SingleSelector]]] =
      for {
        nextSelector <- parser.nextToken().flatMap {
          case JsonPathToken.ValueString =>
            parser.valueAsString.map(string => Attribute(string.value))
          case JsonPathToken.ValueInt =>
            parser.valueAsNumber.map(number => Index(number.value))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              JsonPathToken.ValueString,
              JsonPathToken.ValueInt
            )
        }
        result <- parser.nextToken().flatMap {
          case JsonPathToken.EndSelector => Parsed(acc.addOne(nextSelector))
          case JsonPathToken.Union       => go(acc.addOne(nextSelector))
          case invalidToken =>
            ParseError.invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              JsonPathToken.Union,
              JsonPathToken.EndSelector
            )
        }
      } yield result

    go(Seq.newBuilder[SingleSelector]).map(_.result).flatMap {
      case second :: tail => Parsed(Union(first, second, tail))
      case _ =>
        ParseError(
          s"Trailing '${JsonPathToken.Union}' token at index ${parser.index}.",
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
        case JsonPathToken.EndSelector => Parsed(builder)
        case JsonPathToken.Slice | JsonPathToken.ValueInt
            if builder.knownSize > 3 =>
          ParseError(s"Too many slice arguments.", parser.index, parser.input)
        case JsonPathToken.Slice => go(builder.addOne(None))
        case JsonPathToken.ValueInt =>
          (parser.valueAsNumber.map(_.value), parser.nextToken()).mapN {
            case (value, token) =>
              builder.addOne(Some(value))
              token match {
                case JsonPathToken.EndSelector => Parsed(builder)
                case JsonPathToken.Slice       => go(builder)
                case invalidToken =>
                  ParseError.invalidToken(
                    invalidToken,
                    parser.index,
                    parser.input,
                    JsonPathToken.ValueInt,
                    JsonPathToken.Slice,
                    JsonPathToken.EndSelector
                  )
              }
          }.flatten
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            JsonPathToken.ValueInt,
            JsonPathToken.Slice,
            JsonPathToken.EndSelector
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
        case JsonPathToken.RecursiveDescent =>
          parser.peek().flatMap { nextToken =>
            nextToken
              .fold[ParseResult[RecursiveDescent]](Parsed(RecursiveDescent())) {
                case JsonPathToken.Wildcard | JsonPathToken.ValueInt |
                    JsonPathToken.ValueString =>
                  parseDotSelectorChild().map(RecursiveDescent.apply)
                case JsonPathToken.StartSelector =>
                  parseSelector().map(RecursiveDescent.apply)
                case token => Parsed(RecursiveDescent())
              }
          }
        case invalidToken =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            JsonPathToken.RecursiveDescent
          )
      }

  def parseSelector(): ParseResult[Selector] =
    parser
      .currentToken()
      .fold[ParseResult[Selector]](
        ParseError("Unexpected end of input.", parser.index, parser.input)
      ) {
        case JsonPathToken.StartSelector => parseBracketSelector()
        case JsonPathToken.DotSelector   => parseDotSelectorChild()
        case invalidToken =>
          ParseError
            .invalidToken(
              invalidToken,
              parser.index,
              parser.input,
              JsonPathToken.DotSelector,
              JsonPathToken.StartSelector
            )
      }

  private def parseBracketSelector(): ParseResult[Selector] =
    for {
      firstToken <- parser.nextToken()
      maybeFirstSelector <- Option(firstToken).collect {
        case JsonPathToken.ValueString =>
          parser.valueAsString.map(v => Attribute(v.value))
        case JsonPathToken.ValueInt =>
          parser.valueAsNumber.map(v => Index(v.value))
        case JsonPathToken.Wildcard => Parsed(Wildcard)
        case JsonPathToken.StartExpression =>
          parser.valueAsExpression.map(v => ScriptExpression(v.value))
        case JsonPathToken.StartFilterExpression =>
          parser.valueAsExpression.map(v => FilterExpression(v.value))
      }.sequence
      secondToken <- OptionT
        .fromOption[ParseResult](maybeFirstSelector)
        .semiflatMap(_ => parser.nextToken())
        .value
      selector <- (maybeFirstSelector, firstToken, secondToken) match {
        case (Some(firstSelector), _, Some(JsonPathToken.EndSelector)) =>
          Parsed(firstSelector)
        case (
              Some(singleSelector: SingleSelector),
              _,
              Some(JsonPathToken.Union)
            ) =>
          parseUnion(singleSelector)
        case (Some(Index(index)), _, Some(JsonPathToken.Slice)) =>
          parseSlice(index)
        case (None, JsonPathToken.Slice, None) =>
          parseSlice(None)
        case (_, invalidToken, _) =>
          ParseError.invalidToken(
            invalidToken,
            parser.index,
            parser.input,
            JsonPathToken.EndSelector,
            JsonPathToken.Union,
            JsonPathToken.Slice
          )
      }
    } yield selector

  private def parseDotSelectorChild(): ParseResult[Selector] =
    parser.nextToken().flatMap {
      case JsonPathToken.Wildcard => Parsed(Wildcard)
      case JsonPathToken.ValueString =>
        parser.valueAsString.map(_.value).map(Attribute.apply)
      case JsonPathToken.ValueInt =>
        parser.valueAsNumber.map(_.value).map(Index.apply)
      case invalidToken =>
        ParseError.invalidToken(
          invalidToken,
          parser.index,
          parser.input,
          JsonPathToken.Wildcard,
          JsonPathToken.ValueString,
          JsonPathToken.ValueInt
        )
    }
}

object JsonPathReader {

  def apply(input: String): JsonPathReader =
    new JsonPathReader(JsonPathParser(input))

  def apply(input: String, options: JsonPathParserOptions): JsonPathReader =
    new JsonPathReader(JsonPathParser(input, options))
}
