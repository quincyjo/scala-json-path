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

package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.Expression.ExpressionType

sealed trait ExtensionFunction[Parameters, Result <: ExpressionType]
    extends Expression.Extension {

  def apply(parameters: Parameters): Result

  def name: String
}

object ExtensionFunction {

  trait ExtensionFunction1[
      P1 <: ExpressionType,
      Result <: ExpressionType
  ] extends ExtensionFunction[P1, Result]

  trait ExtensionFunction2[
      P1 <: ExpressionType,
      P2 <: ExpressionType,
      Result <: ExpressionType
  ] extends ExtensionFunction[(P1, P2), Result]

  trait ExtensionFunction3[
      P1 <: ExpressionType,
      P2 <: ExpressionType,
      P3 <: ExpressionType,
      Result <: ExpressionType
  ] extends ExtensionFunction[(P1, P2, P3), Result]

  // TODO: Fully expand to 22 or determine alternate pattern.

  /*
  final case class Length(p: ValueType)
      extends ExtensionFunction1[ValueType, ValueType] {

    override def name: String = "length"
  }

  final case class Value(p: NodesType)
      extends ExtensionFunction1[NodesType, ValueType] {

    override def name: String = "length"
  }
   */
}
