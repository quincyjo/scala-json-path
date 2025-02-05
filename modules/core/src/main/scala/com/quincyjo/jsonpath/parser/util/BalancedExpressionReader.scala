/*
 * Copyright 2023 Quincy Jo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.quincyjo.jsonpath.parser.util

import com.quincyjo.jsonpath.parser.util.Across.across
import com.quincyjo.jsonpath.parser.util.BalancedExpressionReader.ExpressionGroup._

final case class BalancedExpressionReader(input: String) {

  def takeGroup: String = {
    val builder = new StringBuilder()
    across(input)(_.takeWhile)(
      (char, _, isStackEmpty, _) => {
        builder.addOne(char)
        !isStackEmpty
      },
      onTrailingEnd = Iterable.empty
    )
    builder.result()
  }

  /** Takes from the input until the given predicate is true outside any
    * expression group. That is to say that if the predicate alone would be true
    * inside an expression group then it is ignored. This means that the
    * expression must be balanced before the predicate can be met.
    *
    * This differs from
    * [[com.quincyjo.jsonpath.parser.util.BalancedExpressionReader.takeGroup]]
    * in that the first character of the input does not have to begin an
    * expression group.
    *
    * Example:
    * {{{
    * scala> BalancedExpressionReader(
    *     | "This sentence has two periods (one within a group.)."
    *     | ).takeUntil(_== '.')
    * val res1: String = This sentence has two periods (one within a group.)
    * }}}
    *
    * @param until
    *   The predicate to take until.
    * @return
    *   The input taken until the predicate is true.
    */
  def takeUntil(until: Char => Boolean): String = {
    val builder = new StringBuilder()
    across(input)(_.takeWhile)(
      (char, _, isStackEmpty, stackWasEmptied) => {
        val isEnd = isStackEmpty && until(char) && !stackWasEmptied
        if (!isEnd) builder.addOne(char)
        !isEnd
      },
      onTrailingEnd = Iterable.empty
    )
    builder.result()
  }

  def isBalanced: Boolean =
    across(input)(_.forall)(
      (_, index, isStackEmpty, _) => index + 1 < input.length || isStackEmpty,
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
