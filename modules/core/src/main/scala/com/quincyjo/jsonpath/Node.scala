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

/** Represents a node in a JSON document.
  *
  * Identified a specific value within a JSON document. It is a tuple of a
  * [[JsonPath.SingularQuery]] pointing to its location within the root document
  * and the JSON value at that location.
  *
  * @param location
  *   The location of the node within the root document.
  * @param value
  *   The value at the specified location.
  * @tparam Json
  *   The type of the JSON value.
  */
final case class Node[Json](location: JsonPath.SingularQuery, value: Json)
