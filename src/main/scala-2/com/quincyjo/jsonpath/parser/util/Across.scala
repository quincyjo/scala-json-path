package com.quincyjo.jsonpath.parser.util

import com.quincyjo.jsonpath.parser.BalancedExpressionReader.ExpressionGroup

import scala.collection.mutable

object Across {

  def across[Out](input: String)(
      f: Iterable[(Char, Int)] => (((Char, Int)) => Boolean) => Out
  )(map: (Char, Int, Boolean) => Boolean, onTrailingEnd: => Out): Out = {
    val stack = mutable.Stack.empty[ExpressionGroup]
    f(input.zipWithIndex) { case (char, index) =>
      val currentGroup = stack.headOption
      if (currentGroup.exists(_.isValidEndOfGroup(input, index)))
        stack.pop()
      else
        ExpressionGroup.fromChar
          .get(char)
          .filter(
            currentGroup.fold(ExpressionGroup.all)(_.validSubGroups).contains
          )
          .foreach { newGroup =>
            if (newGroup.isStartingCharacter(char)) stack.push(newGroup)
            else if (newGroup.isValidEndOfGroup(input, index))
              return onTrailingEnd
          }
      map(char, index, stack.isEmpty)
    }
  }
}
