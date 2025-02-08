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

import com.quincyjo.jsonpath
import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.JsonSupport.Implicits.JsonSupportOps

import scala.annotation.tailrec
import scala.collection.mutable

abstract class JsonPathEvaluator[Json: JsonSupport] {

  // TODO: Model out a Node and return a list of those instead.
  /** Apply this JsonPath to a JSON, returning a list of JSON values matching
    * this path within the given JSON.
    * @param path
    *   The [[JsonPath]] to evaluate against the given JSON.
    * @param json
    *   The JSON to match against.
    * @return
    *   A list of all matching JSONs within the given JSON.
    */
  final def evaluate(path: JsonPath, json: Json): List[Json] =
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
  final def singular(path: JsonPath.SingularQuery, json: Json): Option[Json] =
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
  ): List[Json] = {
    path.segments.foldLeft(
      path.root match {
        case Root    => List(root)
        case Current => current.toList
      }
    ) { case (values, node) =>
      values.flatMap(step(root, _, node))
    }
  }

  final private[jsonpath] def step(
      root: Json,
      json: Json,
      node: JsonPathSegment
  ): Iterable[Json] =
    node match {
      case RecursiveDescent(selector) =>
        descend(json).flatMap(select(root, _, selector))
      case Child(selector) =>
        select(root, json, selector)
      case Children(selector) =>
        select(root, json, selector)
    }

  final private[jsonpath] def select(
      root: Json,
      json: Json,
      selector: Selector
  ): Iterable[Json] =
    selector match {
      case attribute: Attribute => this.attribute(json, attribute.value)
      case index: Index         => this.index(json, index.value)
      case union: Union         => this.union(root, json, union)
      case slice: Slice         => this.slice(json, slice)
      case filter: Filter       => this.filter(root, json, filter)
      case script: Script       => this.script(root, json, script)
      case Wildcard             => this.wildcard(json)
    }

  final private[jsonpath] def attribute(
      json: Json,
      attribute: String
  ): Iterable[Json] =
    json.asObject.flatMap(_.get(attribute)) orElse
      json.asArray.collect {
        case arr if attribute == "length" =>
          implicitly[jsonpath.JsonSupport[Json]].number(arr.size)
      }

  final private[jsonpath] def index(json: Json, index: Int): Iterable[Json] =
    json.asArray.flatMap(_.lift(index))

  final private[jsonpath] def wildcard(json: Json): Iterable[Json] =
    json.arrayOrObject(Iterable.empty, identity, _.values)

  final private[jsonpath] def union(
      root: Json,
      json: Json,
      union: Union
  ): Iterable[Json] = {
    val builder = Iterable.newBuilder[Json]
    select(root, json, union.head).foreach(builder.addOne)
    select(root, json, union.second).foreach(builder.addOne)
    for (selector <- union.tail)
      yield select(root, json, selector).foreach(builder.addOne)
    builder.result()
  }

  final private[jsonpath] def slice(json: Json, slice: Slice): Iterable[Json] =
    json.asArray.fold(Iterable.empty[Json]) { arr =>
      val roundedStart = slice.start.fold(0) { start =>
        if (start < 0) arr.size + start else start
      }
      val roundedEnd = slice.end.fold(arr.size) { end =>
        if (end < 0) arr.size + end else end
      }
      slice.step
        .fold[Iterable[Json]](arr.slice(roundedStart, roundedEnd)) { step =>
          arr
            .slice(roundedStart, roundedEnd)
            .grouped(step)
            .flatMap(_.headOption)
            .to(Iterable)
        }
    }

  final private[jsonpath] def filter(
      root: Json,
      json: Json,
      filter: Filter
  ): Iterable[Json] =
    json.arrayOrObject(
      Iterable.empty,
      _.filter(j => filter.expression(this, root, j)),
      _.values.filter(j => filter.expression(this, root, j))
    )

  final private[jsonpath] def script(
      root: Json,
      json: Json,
      script: Script
  ): Iterable[Json] =
    script
      .expression(this, root, json)
      .fold[Iterable[Json]](Iterable.empty)(
        _.fold(
          Iterable.empty,
          _ => Iterable.empty,
          i =>
            Option
              .when(i.isValidInt)(i.toInt)
              .flatMap(i => json.asArray.flatMap(_.lift(i))),
          s => json.asObject.flatMap(_.get(s)),
          _ => Iterable.empty,
          _ => Iterable.empty
        )
      )

  final private[jsonpath] def descend(json: Json): List[Json] = {

    @tailrec
    def go(
        builder: mutable.Builder[Json, List[Json]],
        stack: mutable.Stack[Json]
    ): mutable.Builder[Json, List[Json]] =
      if (stack.isEmpty) builder
      else {
        val next = stack.pop()
        stack.pushAll(
          next
            .arrayOrObject(
              Iterable.empty,
              identity,
              _.values
            )
            .filter(_.isAssociative)
        )
        builder.addOne(next)
        go(builder, stack)
      }

    if (json.isAssociative)
      go(List.newBuilder[Json], mutable.Stack(json)).result()
    else
      List(json)
  }
}
