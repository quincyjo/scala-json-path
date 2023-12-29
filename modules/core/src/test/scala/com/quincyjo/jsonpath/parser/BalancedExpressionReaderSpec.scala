package com.quincyjo.jsonpath.parser

import com.quincyjo.jsonpath.parser.BalancedExpressionReader.ExpressionGroup
import com.quincyjo.jsonpath.parser.BalancedExpressionReader.ExpressionGroup._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class BalancedExpressionReaderSpec
    extends AnyFlatSpecLike
    with Matchers
    with TableDrivenPropertyChecks
    with OptionValues {

  "isBalanced" should "handle basic asymmetrical groups" in {
    val groups = ExpressionGroup.all.collect {
      case assymetric: AsymmetricalExpressionGroup =>
        assymetric
    }

    for (group <- groups) yield {
      val start = group.start
      val end = group.end
      val cases = Table(
        ("input", "expected"),
        (s"${start}${end}", true),
        (s"${start}foobar${end}", true),
        (s"${start}foobar${start}deadbeef${end}${end}", true),
        (s"foorbar${start}${end}", true),
        (s"${start}", false),
        (s"${end}", false),
        (s"${start}foobar", false),
        (s"${start}foobardeadbeef${end}${end}", false),
        (s"foorbar${start}", false)
      )

      forAll(cases) { case (input, expected) =>
        BalancedExpressionReader(input).isBalanced should be(expected)
      }
    }
  }

  it should "handle basic escape groups" in {
    val cases = Table(
      "input" -> "expected",
      "'foobar'" -> true,
      "'fo[\"{o(bar'" -> true,
      "'foobar\\'s'" -> true
    )

    forAll(cases) { case (input, expected) =>
      BalancedExpressionReader(input).isBalanced should be(expected)
    }
  }

  "takeGroup" should "take simple groups" in {
    val cases = Table(
      "input" -> "expected",
      "()" -> "()",
      "(foobar)" -> "(foobar)",
      "(foobar)deadbeef" -> "(foobar)",
      "(foobar())deadbeef" -> "(foobar())",
      "[(foobar())deadbeef]" -> "[(foobar())deadbeef]"
    )

    forAll(cases) { case (input, expected) =>
      BalancedExpressionReader(input).takeGroup should be(expected)
    }
  }
}
