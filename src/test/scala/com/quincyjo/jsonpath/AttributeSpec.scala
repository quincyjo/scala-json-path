package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.JsonPath.Attribute
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AttributeSpec
    extends AnyFlatSpecLike
    with Matchers
    with LoneElement
    with TableDrivenPropertyChecks {

  "isSimple" should "determine if the name is dot-chainable" in {
    val cases = Table(
      "right" -> "expected",
      "foobar" -> true,
      "abc123" -> true,
      "1" -> false,
      "123abc" -> false
    )

    forAll(cases) { case (name, expected) =>
      Attribute(name).isSimple should be(expected)
    }
  }

  "quotedName" should "wrap simple names in double quotes" in {
    Attribute("foobar").quotedName should be("\"foobar\"")
  }

  it should "escape double quotes in names" in {
    println(Attribute("\"foobar\"").quotedName)
    Attribute("\"foobar\"").quotedName should be("\"\\\"foobar\\\"\"")
  }

  "toString" should "be just the right for simple names" in {
    Attribute("foobar").toString should be("foobar")
  }

  it should "be quoted for complex names" in {
    Attribute("\"foobar\"").toString should be("\"\\\"foobar\\\"\"")
  }
}
