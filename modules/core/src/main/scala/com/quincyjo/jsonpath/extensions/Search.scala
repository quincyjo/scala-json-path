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

package com.quincyjo.jsonpath.extensions

import com.quincyjo.jsonpath.Expression.{LogicalType, ValueType}
import com.quincyjo.jsonpath.JsonSupport.Implicits.JsonSupportOps
import com.quincyjo.jsonpath.extensions.FunctionExtension.FunctionExtension2
import com.quincyjo.jsonpath.parser.{JsonPathParser, WithExtension}
import com.quincyjo.jsonpath.{Expression, JsonPathEvaluator, JsonSupport}

import scala.util.Try
import scala.util.matching.Regex

/** Tests if the target string contains a substring that matches the given
  * regex. If the target is not a string or the regex is not valid, then the
  * result will be false.
  * @param target
  *   The target value string.
  * @param regex
  *   The regex to search for.
  */
final case class Search(target: ValueType, regex: ValueType)
    extends FunctionExtension2[ValueType, ValueType]
    with LogicalType {

  override val name: String = "search"

  override val args: List[Expression] = List(target, regex)

  override def apply[Json: JsonSupport](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Boolean =
    target(evaluator, root, current)
      .flatMap(_.asString)
      .zip(regex(evaluator, root, current).flatMap(_.asString))
      .exists { case (string, regex) =>
        Try(new Regex(regex)).toOption.exists { regex =>
          regex.findFirstIn(string).isDefined
        }
      }
}

object Search {

  val parser: Extension[(ValueType, ValueType), Search] =
    Extension("search") { args => Search(args._1, args._2) }

  trait SearchExtension extends WithExtension { self: JsonPathParser =>

    addExtension(Search.parser)
  }
}
