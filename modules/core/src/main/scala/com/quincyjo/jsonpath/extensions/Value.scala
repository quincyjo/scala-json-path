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

import com.quincyjo.braid.Braid
import com.quincyjo.jsonpath.Expression.{NodesType, ValueType}
import com.quincyjo.jsonpath.parser.{JsonPathParser, WithExtension}
import com.quincyjo.jsonpath.{Expression, JsonPathEvaluator}

/** Converts a set of nodes to a single JSON value. If a single node is found,
  * then that node is returned. If no nodes are found, then nothing is returned.
  * If more than one node are found, than one of the values wil be returned.
  * Note that in the latter case, which node value is returned is
  * nondeterministic.
  * @param nodes
  *   The nodes to convert to a value.
  */
final case class Value(nodes: NodesType)
    extends FunctionExtension[NodesType]
    with ValueType {

  override val name: String = "value"

  override val args: List[Expression] = List(nodes)

  override def apply[Json: Braid](
      evaluator: JsonPathEvaluator[Json],
      root: Json,
      current: Json
  ): Option[Json] = nodes(evaluator, root, current).headOption.map(_.value)
}

object Value {

  val extension: Extension[NodesType, Value] =
    Extension("value")(Value.apply)

  trait ValueExtension extends WithExtension { self: JsonPathParser =>

    addExtension(Value.extension)
  }
}
