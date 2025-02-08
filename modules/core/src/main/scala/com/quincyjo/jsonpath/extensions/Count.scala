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

import com.quincyjo.jsonpath.Expression.{NodesType, ValueType}
import com.quincyjo.jsonpath.parser.{JsonPathParser, WithExtension}
import com.quincyjo.jsonpath.{Expression, JsonPathEvaluator, JsonSupport}

/** Function that returns the number of nodes that are matched by a given query.
  * @param nodes
  *   The query expression to count.
  */
final case class Count(nodes: NodesType)
    extends FunctionExtension[NodesType]
    with ValueType {

  override val name: String = "count"

  override val args: List[Expression] = List(nodes)

  override def apply[Json: JsonSupport](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Option[Json] = Some(
    implicitly[JsonSupport[Json]].number(
      nodes(evaluator, root, current).size
    )
  )
}

object Count {

  val parser: Extension[NodesType, Count] =
    Extension("count")(Count.apply)

  trait CountExtension extends WithExtension { self: JsonPathParser =>

    addExtension(Count.parser)
  }
}
