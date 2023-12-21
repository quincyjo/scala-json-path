package com.quincyjo.jsonpath.parser

import scala.collection.mutable
import BalancedExpressionReader.*
import BalancedExpressionReader.ExpressionGroup.*
import scala.util.boundary

final case class BalancedExpressionReader(input: String) {

  def takeGroup: String = {
    val builder = new StringBuilder()
    across(_.takeWhile)(
      (char, index, isStackEmpty) => {
        builder.addOne(char)
        !isStackEmpty
      },
      onTrailingEnd = ""
    )
    builder.result()
  }

  def isBalanced: Boolean =
    across(_.forall)(
      (char, index, isStackEmpty) => index + 1 < input.length || isStackEmpty,
      onTrailingEnd = false
    )

  private def across[Out](
      f: Iterable[(Char, Int)] => (((Char, Int)) => Boolean) => Out
  )(map: (Char, Int, Boolean) => Boolean, onTrailingEnd: => Out): Out = {
    val stack = mutable.Stack.empty[ExpressionGroup]
    boundary {
      f(input.zipWithIndex) { case (char, index) =>
        val currentGroup = stack.headOption
        if (currentGroup.exists(isValidEndOfGroup(_, char, index)))
          stack.pop()
        else
          ExpressionGroup.fromChar
            .get(char)
            .filter(
              currentGroup.fold(ExpressionGroup.all)(_.validSubGroups).contains
            )
            .foreach { newGroup =>
              if (newGroup.isStartingCharacter(char)) stack.push(newGroup)
              else if (isValidEndOfGroup(newGroup, char, index))
                boundary.break(onTrailingEnd)
            }
        map(char, index, stack.isEmpty)
      }
    }
  }

  private def isValidEndOfGroup(
      group: ExpressionGroup,
      char: Char,
      index: Int
  ): Boolean =
    group.isEndingCharacter(char) && group.asEscapable
      .zip(input.lift(index - 1))
      .forall { case (escapable, c) =>
        c != escapable.escape
      }
}

object BalancedExpressionReader {

  sealed trait ExpressionGroup {

    def isEscapable: Boolean = false
    def isSymmetrical: Boolean = false

    def asEscapable: Option[EscapableExpressionGroup] = None

    def validSubGroups: Set[ExpressionGroup] = Set.empty

    def isStartingCharacter(char: Char): Boolean
    def isEndingCharacter(char: Char): Boolean
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
