# scala-json-path

`scala-json-path` is a Scala library for the usage of [JSON Path](https://datatracker.ietf.org/doc/rfc9535/#2.3.1). This
library provides a direct ADT for modeling JSONPaths with support for serialization and parsing. In addition, evaluation
of JSON Paths may be done on any underlying JSON library via [Braid](https://github.com/quincyjo/braid).

## Getting Started

To get started, you can add play-json as a dependency in your project:

* sbt
  ```
  libraryDependencies += "com.quincyjo" %% "scala-json-path" % -version-
  ```
* Gradle
  ```
  compile group: 'com.quincyjo', name: 'scala-json-path_2.13', version: -version-
  ```
* Maven
  ```xml
  <dependency>
    <groupId>com.quincyjo</groupId>
    <artifactId>scala-json-path_2.13</artifactId>
    <version>-version-</version>
  </dependency>
  ```

See [GitHub releases](https://github.com/quincyjo/scala-json-path/releases) for the correct version.

Scala Json Path interfaces with the underlying JSON library generically via [Braid](https://github.com/quincyjo/braid).
A `JsonPathEvaluator` can be defined for any `Json` type that has an implicit `Braid` in scope for it.

There are also several pre-defined `JsonPathEvaluator` implementations for common JSON libraries as listed before.

* Modules
  ```
  libraryDependencies += "com.quincyjo" %% "scala-json-path-circe" % -version-
  libraryDependencies += "com.quincyjo" %% "scala-json-path-play" % -version-
  ```

## Usage

### Direct Modeling

JSONPaths may be defined using the ADT API directly, or via a simple DSL.

`JsonPath` has two subtypes, `JsonPath.Query` and `JsonPath.SingularQuery`. The first represents a query which may
resolve to more than one node, and the second is guaranteed to resolve to at most one node. `JsonPath.Query` is not
directly constructable, but the `JsonPath` model handles switching between the two as it is built.

```
scala> JsonPath.$ / "foobar" / 5 
val res0: com.quincyjo.jsonpath.JsonPath.SingularQuery = $['foobar'][5]

scala> JsonPath.$ / Wildcard / Slice.take(3)
val res1: com.quincyjo.jsonpath.JsonPath.Query = $[*][:3]

scala> JsonPath.`@` */ Wildcard // <-- Recursive descent DSL
val res2: com.quincyjo.jsonpath.JsonPath.Query = @..*

cala> JsonPath.$ /? GreaterThan(Count(JsonPathNodes(JsonPath.`@`)), LiteralNumber(1
0)) // <-- Filter DSL
val res3: com.quincyjo.jsonpath.JsonPath.Query = $[?(count(@) > 10)]
```

### Parsing

Parsing is provided via `JsonPathReader`, which reads `JsonPath`s from strings. A direct API is also provided via
the `parser` package object. Parse results are exposed via the sum of `Parsed[T]` and `ParseError`. This library is
built on top of Cats, and `ParseResult` is both a `MonadError` and `Traverse`able.

```
scala> import com.quincyjo.jsonpath

scala> jsonpath.parser.parse("$.foobar[5][-3:][\"1\",1]")
val res0:
  com.quincyjo.jsonpath.parser.ParseResult[com.quincyjo.jsonpath.JsonPath] = Parsed($.foobar[5][-3:]["1",1])
```

#### Escape Sequences

Parsing and serialization of JSONPaths handles escape sequences as specified
in [RFC 9535](https://tools.ietf.org/html/rfc9535) section 3.1.1.

When defining a name selector via the ADT, the provided string is accepted as is and is not processed further. If for
some reason processing of escapes is required on a `String` in code, the API is exposed via the `StringEscapes` object.

```
scala> StringEscapes.processEscapes(s"\\\"") // Literal '\"'
val res0: Either[InvalidStringEncoding, ValueAt[String]] = Right(ValueAt(",0,\"))

scala> JsonPathParser.default.parse("$['\\\"']") // Parse a JSONPath from a string, handling escapes
val res1: ParseResult[JsonPath] = Parsed($['"'])

scala> JsonPath.Attribute("\t") // Raw tab character passed to an Attribute, is not processed.
val res2: JsonPath.Attribute = '\t' // <-- toString escapes it.

scala> JsonPath.Attribute("\\t") // Raw string of `\t` is not processed.
val res3: JsonPath.Attribute = '\\t' // <-- Reverse solidus is escaped
```

### Literals

Literal strings are provided via `jsonpath.literal` package.

```
scala> import com.quincyjo.jsonpath.literal.JsonPathStringContext

scala> jsonPath"""@[1:2:3]["foobar"]"""
val res0: com.quincyjo.jsonpath.JsonPath = @[1:2:3].foobar
```

### Evaluation

Evaluation of a `JsonPath` is performed by a `JsonPathEvaluator`, which is implemented generically via Braid. Once a
[Braid](https://github.com/quincyjo/braid) is in scope for your JSON library of choice, an evaluator may be defined
as below:

```
import JsonBean.jsonBeanBraid // Implicit instance of Braid[JsonBean]

final case object JsonBeanEvaluator extends JsonPathEvaluator[JsonBean]
```

Evaluation returns a `List` of the matching nodes of the path in the given JSON. A node is defined as the tuple its
location via a singular query (a JSON path that points to at most one node) and the value at that location.

```
scala> val json = Json.obj("foobar" -> Json.arr(Json.fromString("deadbeef"), Json.Tr ue, Json.fromInt(42)))
val json: io.circe.Json =
{
  "foobar" : [
    "deadbeef",
    true,
    42
  ]
}

scala> CirceEvaluator.evaluate(jsonPath"$$.foobar", json)
val res0: List[com.quincyjo.jsonpath.Node[io.circe.Json]] =
List(Node($['foobar'],[
  "deadbeef",
  true,
  42
]))

scala> CirceEvaluator.evaluate(jsonPath"$$.foobar.*", json)
val res1: List[com.quincyjo.jsonpath.Node[io.circe.Json]] = List(Node($['foobar'][0],"deadbeef"), Node($['foobar'][1],true), Node($['foobar'][2],42))

scala> CirceEvaluator.evaluate(jsonPath"$$.foobar[-1:]", json)
val res2: List[com.quincyjo.jsonpath.Node[io.circe.Json]] = List(Node($['foobar'][2],42))

scala> CirceEvaluator.evaluate(jsonPath"$$..*", json)
val res3: List[com.quincyjo.jsonpath.Node[io.circe.Json]] =
List(Node($['foobar'],[
  "deadbeef",
  true,
  42
]), Node($['foobar'][0],"deadbeef"), Node($['foobar'][1],true), Node($['foobar'][2],42))
```

Singular queries may also be evaluated explicitly via `JsonPathEvaluator.singular`, which returns `Option[Node[Json]]`
instead.

## Expressions

Expressions have their own AST which can be used to describe expressions in either JSONPath scripts or filters.
Expressions are evaluated against a JsonPath context and return a result based on their expression type. Expressions are
well typed according to [RFC 9535](https://tools.ietf.org/html/rfc9535) section 2.4.3, both at parse time and via AST
declaration.

Functions must be both pure and safe to evaluate, as evaluating a JSON path is guaranteed to be error free. All error
handling is handled when parsing a JSON path, and thus parsing fails if an expression is malformed.

```
scala> jsonPath"$$[?value(@.foo)]"
com.quincyjo.jsonpath.parser.models.ParseError: Failed to parse JsonPath at index 4 in '$[?value(@.foo)]': Filter requires a logical expression but was: value(@['foo'])

scala> $ /? Value(JsonPathValue(`@` / "foo"))
                 ^
       error: type mismatch;
        found   : com.quincyjo.jsonpath.extensions.Value
        required: com.quincyjo.jsonpath.Expression.LogicalType

scala> jsonPath"$$[?match(@.name)]"
com.quincyjo.jsonpath.parser.models.ParseError: Failed to parse JsonPath at index 15 in '$[?match(@.name)]': function 'match' expects 2 arguments, but got 1

scala> jsonPath"$$[?match(@..*, 'deadbeef')]"
com.quincyjo.jsonpath.parser.models.ParseError: Failed to parse JsonPath at index 3 in '$[?match(@..*, 'deadbeef')]': function 'match' invalid argument '@..*': NodesType can only be coerced to ValueType when from a singular query.

scala> $ /? Match(JsonPathNodes(`@` */ Wildcard), LiteralString("deadbeef"))
                               ^
       error: type mismatch;
        found   : com.quincyjo.jsonpath.Expression.JsonPathNodes
        required: com.quincyjo.jsonpath.Expression.ValueType
```

### Types

All expressions have a declared type which is one of the following.

- `ValueType`: A JSON atomic literal value. And evaluates to a single JSON value or nothing, which is represented as
  `Option[Json]` with the `Json` type being determined at evaluation. May be coerced from a singular query.
- `LogicalType`: Either a logical true or false, which is distinct from JSON booleans. May be coerced from any
  `NodesType` as an existence check.
- `NodesType`: Any JSON path, which evaluates to the matching nodes.

Expressions used in a filter selector must be of type `LogicalType`, and function extensions must have a declared
type and their parameters are type-checked during parsing.

There are implicit `def`s in scope for each type to enable implicit conversions according to the above. These will apply
automatically whenever an expression type is used as a parameter. There are also utility methods on relevant types to
make this explicit.

```
scala> JsonPath.$ /? JsonPathNodes(JsonPath.`@` / "bar") // <-- NodesType to Logical Type
val res0: com.quincyjo.jsonpath.JsonPath.Query = $[?(@['bar'])]

scala> JsonPath.$ /? Equal(JsonPathValue(JsonPath.`@` / "name"), LiteralString("Jane Doe")) // <-- NodesType to ValueType
val res1: com.quincyjo.jsonpath.JsonPath.Query = $[?(@['name'] == "Jane Doe")]

scala> JsonPath.$ / "products" */ Wildcard /? JsonPathValue(`@` / "restrictions").exists
val res2: com.quincyjo.jsonpath.JsonPath.Query = $['products']..*[?(@['restrictions'])]
```

Syntax implicits are available via `ExpressionsSyntax` to make writing expressions easier.

```
scala> import com.quincyjo.jsonpath.syntax.ExpressionSyntax._
import com.quincyjo.jsonpath.syntax.ExpressionSyntax._

scala> JsonPath.$ */ "products" /? (`@` / "price" < 100)
val res0: com.quincyjo.jsonpath.JsonPath.Query = $..['products'][?(@['price'] < 100)]
```

## Function Extensions

There is an open trait `FunctionExtension` for defining functions extensions. Any `FunctionExtension` may be used when
defining a `JsonPath`, and may be parsed by mixing in a `WithExtension` for the function extension to a parser.

### Default Function Extensions

The default function extensions are enabled in the default `JsonPathParser`, `JsonPathParser.default`. A mix-in to add
these extensions to any `JsonPathParser` is provided via the `StandardExtensions` trait.

The following extensions are provided by default:

- `Count(NodesType): ValueType`: Counts the number of nodes matched by a given `NodesType`.
- `Length(ValueType): ValueType`: Returns the length of a given `ValueType` if it is a string, array, or object. Nothing
  is returned if the
  operand is not one of the aforementioned types.
- `Value(NodesType): ValueType`: Converts a `NodesType` to a `ValueType`.
- `Match(ValueType, ValueType): LogicalType`: Determines if the first parameter as a string matches the second parameter
  as a regex. If the first parameter is not a string or the second parameter is not a valid regex, then the result is
  false.
- `Search(ValueType, ValueType): LogicalType`: Similar to `Match`, but returns true if the first parameter contains a
  substring which matches the regex.

### Adding Function Extensions

First, define the new function extension. This can immediately be used when defining JSON paths programmatically.

```scala
final case class StringOrNothing(value: ValueType)
  extends FunctionExtension[ValueType]
    with ValueType {

  override val name: String = "stringOrNothing"

  override val args: List[Expression] = List(value)

  override def apply[Json: Braid](
                                   evaluator: JsonPathEvaluator[Json],
                                   root: Json,
                                   current: Json
                                 ): Option[Json] =
    value(evaluator, root, current).asString.map(
      Braid[Json].fromString
    )
}

object StringOrNothing {

  val extension: Extension[NodesType, StringOrNothing] =
    Extension("stringOrNothing")(StringOrNothing.apply)

  trait StringOrNothingExtension extends WithExtension {
    self: JsonPathParser =>

    addExtension(StringOrNothing.extension)
  }
}

```

Then mix it in to a custom parser to be able to parse it.

```scala
case object MyJsonPathParser
  extends JsonPathParser
    with StandardExtensions
    with StringOrNothingExtension

```

### Arithmetic Operations

Arithmetic operations are disabled by default. Similar to function extensions, they may be enabled via a mix-in. This
enables parsing of arithmetic operations including: `+`, `-`, `*`, and `/`.

Plus (`+`) will perform arithmetic summation if both sides are a number or null, otherwise it will coerce both sides to
strings and concatenate them. All other arithmetic operations will attempt to coerce both sides to numbers according to
ES rules and perform the corresponding arithmetic operation if both side are successfully coerced.

```scala
case object MyJsonPathParser extends JsonPathParser with ArithmeticOperations

```

```
scala> case object MyJsonPathParser extends JsonPathParser with ArithmeticOperations

object MyJsonPathParser

scala> val raw = "$[?(@.price + @.tax < 50)]"
val raw: String = $[?(@.price + @.tax < 50)]

scala> JsonPathParser.default.parse(raw)
val res0: com.quincyjo.jsonpath.parser.models.ParseResult[com.quincyjo.jsonpath.JsonPath] = com.quincyjo.jsonpath.parser.models.ParseError: Failed to parse JsonPath at index 12 in '$[?(@.price + @.tax < 50)]': Arithmetic operators are disabled.

scala> MyJsonPathParser.parse(raw)
val res1: com.quincyjo.jsonpath.parser.models.ParseResult[com.quincyjo.jsonpath.JsonPath] = Parsed($[?(@['price'] + @['tax'] < 50)])
```

Because arithmetic operations are not a standard feature, they are not defined within the `Expression` companion object.
Instead, they are available within the `ArithmeticOperations` companion. It is also worth keeping in mind that there is
no defined behavior of these operators within the definition of JSON path, so the behaviour of them may vary depending
on the specific implementation of JSON Path that is being used.

## Example

The below example uses Circe JSON.

```
scala> val json = Json.obj(
     |   "products" -> Json.obj(
     |     "fruit" -> Json.arr(
     |       Json.obj(
     |         "label" -> "Apple".asJson,
     |         "price" -> 2.asJson,
     |         "quantity" -> 15.asJson
     |       ),     
     |       Json.obj(
     |         "label" -> "Banana".asJson,
     |         "price" -> 1.asJson,
     |         "quantity" -> 23.asJson
     |       )
     |     ),
     |     "other" -> Json.arr(
     |       Json.obj(
     |         "label" -> "Dinner Set".asJson,
     |         "price" -> 30.asJson,
     |         "quantity" -> 2.asJson
     |       ),
     |       Json.obj(
     |         "label" -> "Silverware Set".asJson,
     |         "price" -> 10.asJson,
     |         "quantity" -> 4.asJson
     |       )
     |     )
     |   )
     | )
val json: io.circe.Json =
{
  "products" : {
    "fruit" : [
      {
        "label" : "Apple",
        "price" : 2,
        "quantity" : 15
      },
      {
        "label" : "Banana",
        "price" : 1,
        "quantity" : 23
      }
    ],
    "other" : [
      {
        "label" : "Dinner Set",
        "price" : 30,
        "quantity" : 2
      },
      {
        "label" : "Silverware Set",
        "price" : 10,
        "quantity" : 4
      }
    ]
  }
}

scala> val jsonPath = $ / "products" / Wildcard /
     |   Filter(
     |     LessThanOrEqualTo(
     |       JsonPathValue(`@` / "price"),
     |       LiteralNumber(10)
     |     )
     |   )
val jsonPath: com.quincyjo.jsonpath.JsonPath = $.products.*[?(@.price <= 10)]

scala> CirceEvaluator.evaluate(jsonPath, json)
val res0: List[com.quincyjo.jsonpath.Node[io.circe.Json]] =
List(Node($['products']['fruit'][0],{
  "label" : "Apple",
  "price" : 2,
  "quantity" : 15
}), Node($['products']['fruit'][1],{
  "label" : "Banana",
  "price" : 1,
  "quantity" : 23
}), Node($['products']['other'][1],{
  "label" : "Silverware Set",
  "price" : 10,
  "quantity" : 4
}))
```
