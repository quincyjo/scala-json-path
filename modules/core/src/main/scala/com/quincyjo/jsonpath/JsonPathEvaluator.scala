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

import com.quincyjo.braid.Braid
import com.quincyjo.braid.implicits._
import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.JsonPath._

import scala.annotation.tailrec
import scala.collection.mutable

abstract class JsonPathEvaluator[Json: Braid] {

  /** Apply this JsonPath to a JSON, returning a list of JSON values matching
    * this path within the given JSON.
    * @param path
    *   The [[JsonPath]] to evaluate against the given JSON.
    * @param json
    *   The JSON to match against.
    * @return
    *   A list of all matching JSONs within the given JSON.
    */
  final def evaluate(path: JsonPath, json: Json): List[Node[Json]] =
    evaluate(path, json, None)

  /** Evaluates the given [[JsonPath]] against the given JSON as a singular
    * query, that is to say a JSON path that returns a single JSON. If no
    * results or more than one results are found, then None is returned.
    * @param path
    *   The [[JsonPath]] to evaluate against the given JSON.
    * @param json
    *   The JSON to match against.
    * @return
    *   A singular JSON matching the given path or None.
    */
  final def singular(
      path: JsonPath.SingularQuery,
      json: Json
  ): Option[Node[Json]] =
    evaluate(path, json) match {
      case single :: Nil => Some(single)
      case _             => None
    }

  /** Apply the JsonPath to the provided context. Unlike
    * [[evaluate(JsonPath,Json)]], this API is mean for when the root JSON and
    * the current JSON node may not be the same. Namely, this is when a JSON
    * path is part of another JsonPath via an expression.
    * @param path
    *   The [[JsonPath]] to evaluate against the given context.
    * @param root
    *   The root JSON.
    * @param current
    *   The current JSON node of the containing JsonPath.
    * @return
    *   A list of all matching JSONs within the given JSON.
    */
  final private[jsonpath] def evaluate(
      path: JsonPath,
      root: Json,
      current: Option[Json]
  ): List[Node[Json]] =
    path.segments.foldLeft(
      path.root match {
        case Root =>
          List(Node(JsonPath.$, root))
        case Current =>
          current.map { value =>
            Node(JsonPath.`@`, value)
          }.toList
      }
    ) { case (values, segment) =>
      values.flatMap(step(root, _, segment))
    }

  final private[jsonpath] def step(
      root: Json,
      json: Node[Json],
      segment: JsonPathSegment
  ): Iterable[Node[Json]] =
    segment match {
      case RecursiveDescent(selector) =>
        descend(json).flatMap(select(root, _, selector))
      case Child(selector) =>
        singleSelect(json, selector)
      case Children(selector) =>
        multiSelect(root, json, selector)
    }

  final private[jsonpath] def select(
      root: Json,
      json: Node[Json],
      selector: Selector
  ): Iterable[Node[Json]] =
    selector match {
      case selector: SingularSelector => singleSelect(json, selector)
      case selector: MultiSelector    => multiSelect(root, json, selector)
    }

  final private[jsonpath] def singleSelect(
      json: Node[Json],
      selector: SingularSelector
  ): Iterable[Node[Json]] =
    selector match {
      case attribute: Attribute => this.attribute(json, attribute.value)
      case index: Index         => this.index(json, index.value)
    }

  final private[jsonpath] def multiSelect(
      root: Json,
      json: Node[Json],
      selector: MultiSelector
  ): Iterable[Node[Json]] = {
    selector match {
      case union: Union   => this.union(root, json, union)
      case slice: Slice   => this.slice(json, slice)
      case filter: Filter => this.filter(root, json, filter)
      case Wildcard       => this.wildcard(json)
    }
  }

  final private[jsonpath] def attribute(
      json: Node[Json],
      attribute: String
  ): Option[Node[Json]] =
    json.value.asObject.flatMap(_.get(attribute)).map { value =>
      Node(json.location / attribute, value)
    }

  final private[jsonpath] def index(
      json: Node[Json],
      index: Int
  ): Option[Node[Json]] =
    json.value.asArray.flatMap(_.lift(index)).map { value =>
      Node(json.location / index, value)
    }

  final private[jsonpath] def wildcard(json: Node[Json]): Iterable[Node[Json]] =
    json.value.arrayOrObject(
      Iterable.empty,
      _.zipWithIndex.map { case (value, index) =>
        Node(json.location / index, value)
      },
      _.map { case (attribute, value) =>
        Node(json.location / attribute, value)
      }
    )

  final private[jsonpath] def union(
      root: Json,
      json: Node[Json],
      union: Union
  ): Iterable[Node[Json]] = {
    val builder = Iterable.newBuilder[Node[Json]]
    builder.addAll(select(root, json, union.head))
    builder.addAll(select(root, json, union.second))
    for (selector <- union.tail)
      yield select(root, json, selector).foreach(builder.addOne)
    builder.result()
  }

  final private[jsonpath] def slice(
      json: Node[Json],
      slice: Slice
  ): Iterable[Node[Json]] =
    json.value.asArray.fold(Iterable.empty[Node[Json]]) { arr =>
      val roundedStart = slice.start.fold(0) { start =>
        if (start < 0) arr.size + start else start
      }
      val roundedEnd = slice.end.fold(arr.size) { end =>
        if (end < 0) arr.size + end else end
      }
      slice.step
        .fold[Iterable[(Json, Int)]](
          arr.zipWithIndex.slice(roundedStart, roundedEnd)
        ) { step =>
          arr.zipWithIndex
            .slice(roundedStart, roundedEnd)
            .grouped(step)
            .flatMap(_.headOption)
            .to(Iterable)
        }
        .map { case (value, index) => Node(json.location / index, value) }
    }

  final private[jsonpath] def filter(
      root: Json,
      json: Node[Json],
      filter: Filter
  ): Iterable[Node[Json]] =
    json.value.arrayOrObject(
      Iterable.empty,
      _.zipWithIndex
        .filter { case (j, _) => filter.expression(this, root, j) }
        .map { case (j, index) =>
          Node(json.location / index, j)
        },
      _.filter { case (_, j) => filter.expression(this, root, j) }.map {
        case (attribute, j) =>
          Node(json.location / attribute, j)
      }
    )

  final private[jsonpath] def descend(json: Node[Json]): List[Node[Json]] = {

    @tailrec
    def go(
        builder: mutable.Builder[Node[Json], List[Node[Json]]],
        stack: mutable.Stack[Node[Json]]
    ): mutable.Builder[Node[Json], List[Node[Json]]] =
      if (stack.isEmpty) builder
      else {
        val next = stack.pop()
        stack.pushAll(
          next.value
            .arrayOrObject(
              Iterable.empty,
              _.zipWithIndex.map { case (value, index) =>
                Node(next.location / index, value)
              },
              _.map { case (attribute, value) =>
                Node(next.location / attribute, value)
              }
            )
            .filter(_.value.isAssociative)
        )
        builder.addOne(next)
        go(builder, stack)
      }

    if (json.value.isAssociative)
      go(List.newBuilder[Node[Json]], mutable.Stack(json)).result()
    else
      List(json)
  }
}
