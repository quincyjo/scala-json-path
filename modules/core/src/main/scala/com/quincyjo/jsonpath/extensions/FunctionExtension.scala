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

import com.quincyjo.jsonpath.Expression

trait FunctionExtension[Parameters] {
  self: Expression =>

  def name: String

  def args: List[Expression]

  override def toString: String =
    s"$name(${args.mkString(",")})"
}

object FunctionExtension {

  type FunctionExtension2[
      P1 <: Expression,
      P2 <: Expression
  ] = FunctionExtension[(P1, P2)]

  type FunctionExtension3[
      P1 <: Expression,
      P2 <: Expression,
      P3 <: Expression
  ] = FunctionExtension[(P1, P2, P3)]

  type FunctionExtension4[
      P1 <: Expression,
      P2 <: Expression,
      P3 <: Expression,
      P4 <: Expression
  ] = FunctionExtension[(P1, P2, P3, P4)]

  type FunctionExtension5[
      P1 <: Expression,
      P2 <: Expression,
      P3 <: Expression,
      P4 <: Expression,
      P5 <: Expression
  ] = FunctionExtension[(P1, P2, P3, P4, P5)]

  type FunctionExtension6[
      P1 <: Expression,
      P2 <: Expression,
      P3 <: Expression,
      P4 <: Expression,
      P5 <: Expression,
      P6 <: Expression
  ] = FunctionExtension[(P1, P2, P3, P4, P5, P6)]

  // TODO: Fully expand to 22 or determine alternate pattern, EG Shapeless
}
