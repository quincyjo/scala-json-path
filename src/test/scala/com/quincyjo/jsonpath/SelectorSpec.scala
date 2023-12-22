package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.JsonPath.{
  Attribute,
  Index,
  Slice,
  Union,
  Wildcard
}
import org.scalatest.{LoneElement, OptionValues}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class SelectorSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks
    with LoneElement
    with OptionValues {

  "Index" should "select an array's index" in {
    val givenIndex = 0
    val givenIndexValue = JsonBean.string("Test attribute value!")
    val givenJson = JsonBean.arr(
      givenIndexValue
    )
    Index(givenIndex)(givenJson).loneElement should be(givenIndexValue)
  }

  it should "return none if the given index does not exist" in {
    val givenIndex = 0
    val givenJson = JsonBean.arr()

    Index(givenIndex)(givenJson) should be(empty)
  }

  it should "return none if the given json is not an array" in {
    val givenIndex = 0
    val cases = Table(
      "json",
      JsonBean.number(1),
      JsonBean.True,
      JsonBean.string("foobar"),
      JsonBean.obj()
    )

    forAll(cases) { givenJson =>
      Index(givenIndex)(givenJson) should be(empty)
    }
  }

  "Wildcard" should "return nothing on atomic values" in {
    val cases = Table(
      "json",
      JsonBean.number(1),
      JsonBean.True,
      JsonBean.string("foobar")
    )

    forAll(cases) { json =>
      Wildcard(json) should be(empty)
    }
  }

  it should "return the values of an array" in {
    val values = Vector(
      JsonBean.number(1),
      JsonBean.string("foobar"),
      JsonBean.False,
      JsonBean.obj("deadbeef" -> JsonBean.True),
      JsonBean.arr(JsonBean.True)
    )

    Wildcard(JsonBean.fromValues(values)) should contain theSameElementsAs values
  }

  it should "return the attribute values of an object" in {
    val values = Vector(
      JsonBean.number(1),
      JsonBean.string("foobar"),
      JsonBean.False,
      JsonBean.obj("deadbeef" -> JsonBean.True),
      JsonBean.arr(JsonBean.True)
    )

    Wildcard(JsonBean.fromAttributes(values.zipWithIndex.map { case (value, index) =>
      index.toString -> value
    })) should contain theSameElementsAs values
  }

  "Union" should "select indices from arrays" in {
    val givenJson = JsonBean.fromValues(
      Seq.tabulate(5)(JsonBean.number)
    )
    val targetIndices = Seq(0, 2, 4)
    val union = Union(targetIndices.head, targetIndices(1), targetIndices(2))

    union(givenJson) should contain theSameElementsAs targetIndices.map(
      JsonBean.number
    )
  }

  it should "select target attributes from objects" in {
    val raw = Map(
      "a" -> JsonBean.number(0),
      "b" -> JsonBean.number(1),
      "c" -> JsonBean.number(2),
      "d" -> JsonBean.number(3),
      "e" -> JsonBean.number(4)
    )
    val givenJson: JsonBean = JsonBean.JObject(raw)
    val targetAttributes = Seq("a", "c", "e")
    val union =
      Union(targetAttributes.head, targetAttributes(1), targetAttributes(2))

    union(givenJson) should contain theSameElementsAs targetAttributes.map(raw)
  }

  "Slice" should "drop elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.drop(2)(givenJson) should contain theSameElementsAs elements.drop(2)
  }

  it should "reflect the array with a start of 0" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.start(0)(givenJson) should contain theSameElementsAs elements
  }

  it should "take elements from an array" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.take(2)(givenJson) should contain theSameElementsAs elements.take(2)
  }

  it should "take right with a negative start" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.start(-2)(givenJson) should contain theSameElementsAs elements
      .takeRight(2)
  }

  it should "drop right with a negative end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.end(-2)(givenJson) should contain theSameElementsAs elements
      .dropRight(2)
  }

  it should "take a sub array with start and end" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    Slice.apply(2, 4)(givenJson) should contain theSameElementsAs elements
      .slice(2, 4)
  }

  it should "take every Nth element via step" in {
    val numberOfElements = 5
    val elements = Seq.tabulate(numberOfElements)(JsonBean.number)
    val givenJson = JsonBean.fromValues(elements)

    val cases = Table(
      "step" -> "expected elements",
      1 -> elements,
      2 -> Seq(0, 2, 4).map(elements),
      3 -> Seq(0, 3).map(elements),
      4 -> Seq(0, 4).map(elements),
      5 -> Seq(0).map(elements)
    )

    forAll(cases) { case (step, expected) =>
      Slice.everyN(step)(givenJson) should contain theSameElementsAs expected
    }
  }
}
