/*
 * Copyright 2023 Typelevel
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

import BalancedExpressionReader.ExpressionGroup

import scala.collection.mutable
import scala.util.boundary

private[parser] object Across {

  def across[Out](input: String)(
      f: Iterable[(Char, Int)] => (((Char, Int)) => Boolean) => Out
  )(map: (Char, Int, Boolean) => Boolean, onTrailingEnd: => Out): Out = {
    val stack = mutable.Stack.empty[ExpressionGroup]
    boundary {
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
                boundary.break(onTrailingEnd)
            }
        map(char, index, stack.isEmpty)
      }
    }
  }
}
