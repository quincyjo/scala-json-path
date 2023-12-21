# scala-json-path

`scala-json-path` is a Scala library for usage of [JSONPath](https://goessner.net/articles/JsonPath/). This library
provides a direct ADT for modeling JSONPaths with support for serialization and parsing. In addition, evaluation of
JSONPaths may be done on any type of `Json` which provides support via `JsonSupport` API.

## Usage

### Direct Modeling

JSONPaths may be defined using the ADT API directly, or via a simple DSL.

```scala
scala> import com.quincyjo.jsonpath.JsonPath

scala> JsonPath(JsonPath.Root, JsonPath.Child("foobar"), JsonPath.Child(5), JsonPath.Child(JsonPath.Slice.takeRight(3)), JsonPath.Child.Union("1", 1))
val res0: com.quincyjo.jsonpath.JsonPath = $.foobar[5][-3:]["1",1]

scala> JsonPath.$ / "foobar" / 5 / JsonPath.Slice.takeRight(3) / JsonPath.Union("1", 1)
val res1: com.quincyjo.jsonpath.JsonPath = $.foobar[5][-3:]["1",1]
```

### Parsing

Parsing is provided via `JsonPathReader`, which reads `JsonPath`s from strings. A direct API is also provided via
the `parser` package object. Parse results are exposed via the sum of `Parsed[T]` and `ParseError`.

```scala
scala> import com.quincyjo.jsonpath

scala> jsonpath.parser.parse("$.foobar[5][-3:][\"1\",1]")
val res0:
  com.quincyjo.jsonpath.parser.ParseResult[com.quincyjo.jsonpath.JsonPath] = Parsed($.foobar[5][-3:]["1",1])
```

### Literals

Literal strings are provided via `jsonpath.literal` package.

```scala
scala> import com.quincyjo.jsonpath.literal.JsonPathStringContext

scala> jsonPath"""@[1:2:3]["foobar"]"""
val res0: com.quincyjo.jsonpath.JsonPath = @[1:2:3].foobar
```

### Evaluation

A `JsonPath` may be applied to any `Json` with evidence of `JsonSupport` and will return a `List` of the matching values
to the path in the given JSON. This call is safe with the exception of expressions. See the [Expressions](#Expressions)
section for more details on expressions.

NOTE: This API and behavior may be adjusted in the future to avoid exception entirely, but as most JSONPaths are safe,
the initial draft API does not model this failure.

```scala
scala> val json = JsonBean.obj("foobar" -> JsonBean.arr(JsonBean.string("deadbeef"), JsonBean.True, JsonBean.number(42)))
val json: JsonBean = { "foobar": ["deadbeef" , true , 42 ] }

scala> jsonPath"""$$.foobar"""(json)
val res0: List[JsonBean] = List(["deadbeef", true, 42])

scala> jsonPath"""$$["foobar"].*"""(json)
val res1: List[JsonBean] = List("deadbeef", true, 42)

scala> jsonPath"""$$.foobar[-1:]"""(json)
val res2: List[JsonBean] = List(42)

scala> jsonPath"""$$..*"""(json)
val res3: List[JsonBean] = List(["deadbeef", true, 42] , "deadbeef" , true , 42)
```

## Expressions

By default, expressions (eg `?(@.length>1)`) are parsed as literal expressions, and cannot be evaluated. Expressions are
parsed based on the statement being balanced in terms of basic blocks; parentheses (`()`), curly brackets (`{}`), square
brackets (`[]`), single quotes (`'`), and double quotes (`"`). Parsing and serialization of expressions will work by
default, but if a `JsonPath` contains a `LiteralExpression` or other `NonExecutableExpression`, then evaluation will
result in a `UnsupportedOperationException` exception.

```scala
scala> val jsonPath = jsonPath"""$$[(@.*.length>0)]"""
val jsonPath: com.quincyjo.jsonpath.JsonPath = $[( @.*.length>0)]

scala > jsonPath(JsonBean.obj())
java.lang.UnsupportedOperationException: Cannot execute non-executable expression '@.*.length>0'. In order to support executing evaluation of script expressions , provide an ExpressionParser via JsonPathParserOptions which parses ExecutableExpressions.
```

Custom instances of expressions may be defined, as well as an associated expression parser. The expression parser may be
provided to a parser to parse executable expressions to allow evaluation of expressions. A alternate expression parser
is provided that parses json paths alone.

```scala
scala> import com.quincyjo.jsonpath.parser.*

scala> val json = JsonBean.arr(Seq.tabulate(6) { n =>
     |   JsonBean.obj("keep" -> (if (n % 2 == 0) JsonBean.True else JsonBean.False))
     | }: _*)
val json: JsonBean = [ { "keep": true }, { "keep": false }, { "keep": true }, { "keep": false }, { "keep": true }, { "keep": false } ]

scala> val jsonPath = JsonPathReader("$[?(@.keep)]", JsonPathParser.JsonPathParserOptions(expressionParser = ExpressionParser.JsonPathExpressionParser)).parseInput().get
val jsonPath: com.quincyjo.jsonpath.JsonPath = $[?(@.keep)]

scala> jsonPath(json)
val res0: List[JsonBean] = List({ "keep": true }, { "keep": true }, { "keep": true })
```

## Variance from Core Library

There are a few small variances from the behavior of the core Javascript library in some edge cases.
These variances in behavior are intentional, as they remove ambiguity and are more precise.

### Quoted Strings

In the core library, quotes around strings, namely in attribute selection, are only respected at either end of a bracket
selector (`[<selector>]`). Furthermore, these quotations do not have to be closed or even match. If the opening
branch (`[`) of a selector is followed immediately by either a single quote (`'`) or double quote (`"`), the quote is
dropped. And similarly for a quote immediately preceding a closing bracket (`]`) of a selector.

For example:

```
Core Library:
["foobar"] -> .foobar
[foobar"] -> .foobar
["foobar] -> .foobar
["foobar'] -> .foobar

[""foobar"'] -> ["foobar"]

This Library:
["foobar"] -> .foobar
['foobar'] -> .foobar
[foobar"] -> invalid, parse fails
["foobar] -> invalid, parse fails
["foobar'] -> invalid, parse fails

["\"foobar\""] or ['"foobar"'] -> ["\"foobar\""] as in an object with an attribute '"foobar"' 
```

Serialization also respects quotes, and will escape quotes that are nested in the string. As the library always
serializaed strings in double quotes, only double quotes within a selector will be escaped.

```scala
scala > JsonPath.Child("\"Proper Noun\"")
val res0: com.quincyjo.jsonpath.JsonPath.Child = ["\"Proper Noun\""]
```

### Union String Quotes

As quotes are only respected (or rather ignored) if immediately following a selector opening bracket (`[`]) or a
selector closing bracket (`]`), individual values of a union selector or not quoted. In this library, each individual
selector within the union has its own quotations and escaps handled individually.