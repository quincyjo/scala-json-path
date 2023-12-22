package com.quincyjo.jsonpath.parser

import BalancedExpressionReader.ExpressionGroup._
import util.Across.across

final case class BalancedExpressionReader(input: String) {

  def takeGroup: String = {
    val builder = new StringBuilder()
    across(input)(_.takeWhile)(
      (char, index, isStackEmpty) => {
        builder.addOne(char)
        !isStackEmpty
      },
      onTrailingEnd = Iterable.empty
    )
    builder.result()
  }

  def isBalanced: Boolean =
    across(input)(_.forall)(
      (char, index, isStackEmpty) => index + 1 < input.length || isStackEmpty,
      onTrailingEnd = false
    )

}

object BalancedExpressionReader {

  sealed trait ExpressionGroup {

    def isEscapable: Boolean = false
    def isSymmetrical: Boolean = false

    def asEscapable: Option[EscapableExpressionGroup] = None

    def validSubGroups: Set[ExpressionGroup] = Set.empty

    def isStartingCharacter(char: Char): Boolean
    def isEndingCharacter(char: Char): Boolean

    def isValidEndOfGroup(input: String, index: Int): Boolean =
      input.lift(index).forall(isEndingCharacter) && asEscapable
        .zip(input.lift(index - 1))
        .forall { case (escapable, c) =>
          c != escapable.escape
        }
  }

  object ExpressionGroup {

    sealed trait SymmetricalExpressionGroup extends ExpressionGroup {

      override val isSymmetrical: Boolean = true

      def designator: Char

      override def isStartingCharacter(char: Char) = char == designator

      override def isEndingCharacter(char: Char) = char == designator
    }

    sealed trait AsymmetricalExpressionGroup extends ExpressionGroup {

      override val isSymmetrical: Boolean = false

      def start: Char

      def end: Char

      override def isStartingCharacter(char: Char) = char == start

      override def isEndingCharacter(char: Char) = char == end
    }

    sealed trait EscapableExpressionGroup extends ExpressionGroup {

      override val isEscapable: Boolean = true

      override def asEscapable: Option[EscapableExpressionGroup] = Some(this)

      def escape: Char
    }

    private case object Parentheses
        extends ExpressionGroup
        with AsymmetricalExpressionGroup {
      override val start = '('
      override val end = ')'

      override def validSubGroups = Set(
        Parentheses,
        SquareBrackets,
        CurlyBraces,
        SingleQuotes,
        DoubleQuotes
      )
    }

    private case object SquareBrackets
        extends ExpressionGroup
        with AsymmetricalExpressionGroup {
      override val start = '['
      override val end = ']'

      override def validSubGroups = Set(
        Parentheses,
        SquareBrackets,
        CurlyBraces,
        SingleQuotes,
        DoubleQuotes
      )
    }

    private case object CurlyBraces
        extends ExpressionGroup
        with AsymmetricalExpressionGroup {
      override val start = '{'
      override val end = '}'

      override def validSubGroups = Set(
        Parentheses,
        SquareBrackets,
        CurlyBraces,
        SingleQuotes,
        DoubleQuotes
      )
    }

    private case object SingleQuotes
        extends ExpressionGroup
        with SymmetricalExpressionGroup
        with EscapableExpressionGroup {
      override val designator = '\''
      override val escape = '\\'
    }

    private case object DoubleQuotes
        extends ExpressionGroup
        with SymmetricalExpressionGroup
        with EscapableExpressionGroup {
      override val designator = '"'
      override val escape = '\\'
    }

    val all: Set[ExpressionGroup] =
      Set(Parentheses, SquareBrackets, CurlyBraces, SingleQuotes, DoubleQuotes)

    val fromChar: Map[Char, ExpressionGroup] = all.flatMap {
      case group: SymmetricalExpressionGroup => Set(group.designator -> group)
      case group: AsymmetricalExpressionGroup =>
        Set(group.start -> group, group.end -> group)
    }.toMap
  }

}
