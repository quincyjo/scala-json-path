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

import com.quincyjo.jsonpath.JsonPath.JsonPathRoot.{Current, Root}
import com.quincyjo.jsonpath.JsonPath.SingleSelector.SingleSelectorWrapper
import com.quincyjo.jsonpath.JsonPath._
// import scala.annotation.{tailrec, targetName}

final case class JsonPath(
    root: Option[JsonPathRoot],
    path: List[JsonPathNode]
) {

  /** Returns true if this path is absolute, IE; if it has is rooted in the root
    * document.
    * @return
    *   A boolean indicating if this path is absolute.
    */
  def isAbsolute: Boolean = root.contains(Root)

  /** Returns true if this path is dynamic, IE; if it is rooted in the current
    * document.
    * @return
    *   A boolean indicating if this path is dynamic.
    */
  def isDynamic: Boolean = root.contains(Current)

  /** Returns true if this path is relative, IE; if it has not root.
    * @return
    *   A boolean indicating if this path is relative.
    */
  def isRelative: Boolean = root.isEmpty

  /** Appends the given [[JsonPath.JsonPathNode]] to this path.
    * @param that
    *   The [[JsonPath.JsonPathNode]] to append.
    * @return
    *   This path with the given node appended.
    */
  def appended(that: JsonPathNode): JsonPath =
    JsonPath(root, path appended that)

  /** Appends all of the given [[JsonPath.JsonPathNode]] s to this path.
    * @param that
    *   An iterable of the [[JsonPath.JsonPathNode]] s to append.
    * @return
    *   This path with the given nodes appended.
    */
  def appendedAll(that: Iterable[JsonPathNode]): JsonPath =
    copy(path = path concat that)

  /** Alias for [[appendedAll]].
    * @param that
    *   An iterable of the [[JsonPath.JsonPathNode]] s to append.
    * @return
    *   This path with the given nodes appended.
    * @see
    *   [[appendedAll]].
    */
  def concat(that: Iterable[JsonPathNode]): JsonPath =
    appendedAll(that)

  /** Prepends the given [[JsonPath.JsonPathNode]] to this path.
    * @param that
    *   The [[JsonPath.JsonPathNode]] to prepend.
    * @return
    *   This path with the given node prepended.
    */
  def prepended(that: JsonPathNode): JsonPath =
    JsonPath(root, path prepended that)

  /** Prepends all of the given [[JsonPath.JsonPathNode]] s to this path.
    * @param that
    *   An iterable of the [[JsonPath.JsonPathNode]] s to prepend.
    * @return
    *   This path with the given nodes prepended.
    */
  def prependedAll(that: Iterable[JsonPathNode]): JsonPath =
    copy(path = path concat that)

  /** DSL to append a child [[JsonPath.Selector]] to this [[JsonPath]].
    * @param singleSelectorWrapper
    *   A wrapped [[JsonPath.SingleSelector]] right.
    * @return
    *   This path with the given selector appended.
    */
  // @targetName("select")
  def /(singleSelectorWrapper: SingleSelectorWrapper): JsonPath =
    appended(Property(singleSelectorWrapper.value))

  /** DSL to append a child [[JsonPath.Selector]] to this [[JsonPath]].
    * @param selector
    *   The [[JsonPath.Selector]] to append.
    * @return
    *   This path with the given selector appended.
    */
  // @targetName("select")
  def /(selector: Selector): JsonPath =
    appended(Property(selector))

  /** DSL for appending many [[JsonPath.JsonPathNode]] s to this path. Alias for
    * [[appendedAll]]
    * @param path
    *   An iterable of the [[JsonPath.JsonPathNode]] s to append.
    * @return
    *   This path with the given nodes appended.
    * @see
    *   [[appendedAll]]
    */
  // @targetName("add")
  def /(path: Iterable[JsonPathNode]): JsonPath =
    appendedAll(path)

  /** DSL for appending [[JsonPath.JsonPathNode]] s to this path. Alias for
    * [[appended]].
    * @param node
    *   The [[JsonPath.JsonPathNode]] to append.
    * @return
    *   This path with the given node appended.
    * @see
    *   [[appended]]
    */
  // @targetName("add")
  def /(node: JsonPathNode): JsonPath =
    appended(node)

  /** Takes the first `n` nodes of this path.
    * @param n
    *   The number of nodes to take.
    * @return
    *   This path truncated to the first `n` [[JsonPath.JsonPathNode]] s.
    */
  def take(n: Int): JsonPath = copy(path = path.take(n))

  /** Drops the first `n` nodes of this path.
    * @param n
    *   The number of nodes to drop.
    * @return
    *   This path with the first `n` [[JsonPath.JsonPathNode]] s dropped.
    */
  def drop(n: Int): JsonPath = copy(path = path.drop(n))

  /** Takes the last `n` nodes of this path.
    * @param n
    *   The number of nodes to take.
    * @return
    *   This path truncated to the last `n` [[JsonPath.JsonPathNode]] s.
    */
  def takeRight(n: Int): JsonPath = copy(path = path.takeRight(n))

  /** Drops the last `n` nodes of this path.
    * @param n
    *   The number of nodes to drop.
    * @return
    *   This path with the last `n` [[JsonPath.JsonPathNode]] s dropped.
    */
  def dropRight(n: Int): JsonPath = copy(path = path.dropRight(n))

  /** Returns true if this path is empty and false otherwise. The root node of
    * the path is not considered.
    * @return
    *   True if this path is empty, false otherwise.
    */
  def isEmpty: Boolean = path.isEmpty

  /** Returns true if this path is not empty and false otherwise. The root node
    * of the path is not considered.
    * @return
    *   True if this path is not empty, false otherwise.
    */
  def nonEmpty: Boolean = path.nonEmpty

  /** Returns the number of [[JsonPath.JsonPathNode]] in this path.
    * @return
    *   The number of [[JsonPath.JsonPathNode]] in this path.
    */
  def size: Int = path.size

  /** Returns the length of this path.
    * @return
    *   The length of this path.
    */
  def length: Int = path.length

  /** Returns true if this path has a parent and false otherwise.
    * @return
    *   True if this path has a parent, false otherwise.
    */
  def hasParent: Boolean = nonEmpty

  /** Returns the parent path of this path, or an empty path if this path is
    * empty. For example, if this path is `$.foo.bar`, this function would
    * return `$.foo`.
    * @return
    *   The parent path of this path.
    */
  def parent: JsonPath = dropRight(1)

  /** Resolve the given [[JsonPath]] against this one. If the given path is not
    * relative, then then the given path is simply returned. Otherwise, the
    * given path is appended to this path. For example, if this path is `$.foo`
    * and the given path is `.bar[1,2,3]`, then `$.foo.bar[1,2,3]` is returned.
    * If the given path is `$.deadbeef`, then `$.deadbeef` is returned
    * regardless of whatever this path is.
    * @param that
    *   The path to resolve against this one.
    * @return
    *   The resolved path.
    */
  def resolve(that: JsonPath): JsonPath =
    if (!that.isRelative) that
    else appendedAll(that.path)

  /** Resolve the given [[JsonPath]] against this one as a sibling. If the given
    * path is not relative or if this path has no parent, then then the given
    * path is simply returned. Otherwise, the given path is appended to this
    * path's parent path. For example, if this path is `$.foo.bar` and the given
    * path is `.deadbeef[1,2,3]`, then `$.foo.deadbeef[1,2,3]` is returned. If
    * the given path is `$.deadbeef`, then `$.deadbeef` is returned regardless
    * of whatever this path is. And if this path is `$` then the given path is
    * returned.
    * @param that
    *   The path to resolve as this one's sibling.
    * @return
    *   The resolved sibling path.
    */
  def resolveSibling(that: JsonPath): JsonPath =
    if (!hasParent) that
    else parent.resolve(that)

  /** Returns this path as an absolute path, IE, with a [[root]] of
    * [[JsonPath.JsonPathRoot.Root]].
    * @return
    *   This path as an absolute path.
    */
  def toAbsolutePath: JsonPath =
    if (isAbsolute) this
    else copy(root = Some(Root))

  /** Returns this path as a relative path, ie with no [[root]].
    * @return
    *   This path as a relative path.
    */
  def toRelativePath: JsonPath =
    if (isRelative) this
    else copy(root = None)

  /** Returns this path as an absolute path, IE, with a [[root]] of
    * [[JsonPath.JsonPathRoot.Current]].
    * @return
    *   This path as a dynamic path.
    */
  def toDynamicPath: JsonPath =
    if (isDynamic) this
    else copy(root = Some(Current))

  override def toString: String =
    root.fold(path.mkString) { root =>
      s"$root${path.mkString}"
    }
}

object JsonPath {

  final val $ : JsonPath = JsonPath(Some(Root), List.empty)
  final val `@` : JsonPath = JsonPath(Some(Current), List.empty)
  final val empty: JsonPath = JsonPath(None, List.empty)

  def apply(
      jsonPathHead: JsonPathRoot,
      jsonPathNodes: JsonPathNode*
  ): JsonPath =
    JsonPath(Some(jsonPathHead), jsonPathNodes.toList)

  def apply(jsonPathNodes: JsonPathNode*): JsonPath =
    JsonPath(None, jsonPathNodes.toList)

  sealed trait JsonPathNode

  sealed trait JsonPathRoot

  object JsonPathRoot {

    case object Root extends JsonPathRoot {
      override def toString: String = "$"
    }

    case object Current extends JsonPathRoot {
      override def toString: String = "@"
    }

  }

  /** Recursively descends through exposing all associative JSON nodes
    * throughout the target right. If a selector is provided, then that selector
    * will be applied to each associative discovered by the recursive descent.
    *
    * @param selector
    *   An optional selector to apply to all matches.
    */
  final case class RecursiveDescent(selector: Option[Selector] = None)
      extends JsonPathNode {

    override def toString: String = selector.fold("..") {
      case attribute: Attribute if attribute.isSimple => s"..$attribute"
      case Wildcard                                   => s"..$Wildcard"
      case selector                                   => s"..[$selector]"
    }
  }

  object RecursiveDescent {

    def apply(selector: Selector): RecursiveDescent =
      new RecursiveDescent(Some(selector))

    def apply(selector: SingleSelectorWrapper): RecursiveDescent =
      apply(selector.value)
  }

  /** [[JsonPathNode]] that applies a [[Selector]] to a JSON right to match zero
    * or more of its leaf nodes.
    * @param selector
    *   The [[Selector]] which this node contains.
    */
  final case class Property(selector: Selector) extends JsonPathNode {

    override def toString: String = selector match {
      case attribute: Attribute if attribute.isSimple => s".$attribute"
      case Wildcard                                   => s".$Wildcard"
      case selector                                   => s"[$selector]"
    }
  }

  object Property {

    def apply(selector: SingleSelectorWrapper): Property =
      new Property(selector.value)

    def apply(
        selector: SingleSelectorWrapper,
        selector2: SingleSelectorWrapper,
        selectors: SingleSelectorWrapper*
    ): Property =
      new Property(Union(selector, selector2, selectors: _*))

    def attribute(attribute: String): Property =
      new Property(Attribute(attribute))

    def index(index: Int): Property =
      new Property(Index(index))
  }

  /** A description of a selection of properties of a JSON right, such as an
    * right, attribute, slice, etc.
    */
  sealed trait Selector

  /** A selector which describes a single property selection, and is not
    * composed of other selectors. This includes selection by attribute right,
    * right, or wildcard. Single selectors may be composed into a union.
    */
  sealed trait SingleSelector extends Selector {

    /** Creates a union between this single selector and another.
      * @param that
      *   The second selector.
      * @return
      *   A union of this selector and the other.
      */
    def or(that: SingleSelector): Union = Union(this, that)
  }

  object SingleSelector {

    sealed trait SingleSelectorWrapper {
      def value: SingleSelector
    }

    private case class SingleSelectorWrapperImpl(field: SingleSelector)
        extends SingleSelectorWrapper {
      override def value: SingleSelector = field
    }

    implicit def toJsFieldSingleSelectorWrapper[T](field: T)(implicit
        w: SingleSelectorMagnet[T]
    ): SingleSelectorWrapper =
      SingleSelectorWrapperImpl(w(field))

    sealed trait SingleSelectorMagnet[T] {
      def apply(t: T): SingleSelector
    }

    object SingleSelectorMagnet {

      implicit case object AttributeMagnet
          extends SingleSelectorMagnet[String] {

        override def apply(t: String): SingleSelector = Attribute(t)
      }

      implicit case object IndexMagnet extends SingleSelectorMagnet[Int] {

        override def apply(t: Int): SingleSelector = Index(t)
      }
    }
  }

  /** Selects the given attribute by right from a JSON object.
    * @param value
    *   The attribute right to select.
    */
  final case class Attribute(value: String) extends SingleSelector {

    /** Returns true if the right is a simple identifier, meaning that it only
      * contains letters and digits.
      * @return
      *   True if the right is a simple identifier or false otherwise.
      */
    def isSimple: Boolean =
      value.headOption.forall(_.isLetter) && value.forall(_.isLetterOrDigit)

    def quotedName: String = s"""\"${value.replace("\"", "\\\"")}\""""

    override def toString: String =
      if (isSimple) value
      else quotedName
  }

  object Attribute {

    final val length = Attribute("length")
  }

  /** Selects the given right from a JSON array.
    * @param value
    *   The right to select.
    */
  final case class Index(value: Int) extends SingleSelector {

    override def toString: String = value.toString
  }

  /** Selects all direct children of an associative JSON right. If the target
    * right is atomic, nothing is matched.
    */
  case object Wildcard extends SingleSelector {

    override def toString: String = "*"
  }

  /** Selects the union of two or more [[SingleSelector]] s, ie, this
    * [[Selector]] selects values which match any of the given
    * [[SingleSelector]] s.
    * @param head
    *   The first [[SingleSelector]] in the union.
    * @param second
    *   The second [[SingleSelector]] in the union.
    * @param tail
    *   The remaining [[SingleSelector]] s in the union.
    */
  final case class Union(
      head: SingleSelector,
      second: SingleSelector,
      tail: Seq[SingleSelector] = Seq.empty
  ) extends Selector {

    def or(that: SingleSelector): Union =
      appended(that)

    def appended(that: SingleSelector): Union =
      copy(tail = tail.appended(that))

    // @targetName("add")
    def +(that: SingleSelector): Union =
      appended(that)

    override def toString: String =
      tail
        .prepended(second)
        .prepended(head)
        .mkString(",")
  }

  object Union {

    def apply(
        head: SingleSelectorWrapper,
        second: SingleSelectorWrapper,
        tail: SingleSelectorWrapper*
    ): Union =
      new Union(head.value, second.value, tail.map(_.value))
  }

  /** Selects a slice of a JSON array. If the target JSON is not an array,
    * nothing is selected. At least one of `start`, `end`, and `step` must be
    * specified.
    * @param start
    *   The start of the slice, inclusive. If negative, it is relative to the
    *   end of the array.
    * @param end
    *   The end of the slice, exclusive. If negative, it is relative to the end
    *   of the array.
    * @param step
    *   The step of the slice. EG, a slice of `::2` would select every other
    *   element.
    */
  final case class Slice private (
      start: Option[Int],
      end: Option[Int],
      step: Option[Int]
  ) extends Selector {

    override def toString: String =
      Seq(
        start.fold("")(_.toString),
        end.fold("")(_.toString)
      ).mkString(":") + step.fold("")(step => s":$step")
  }

  object Slice {

    /** Creates a slice with the given start right, ie, the subarray from the
      * given idnex.
      * @param int
      *   The start of the slice.
      * @return
      *   The slice.
      */
    def start(int: Int): Slice = new Slice(Some(int), None, None)

    /** Creates a slice with the given end right, ie, the subarray up to the
      * given right.
      * @param int
      *   The end of the slice.
      * @return
      *   The slice.
      */
    def end(int: Int): Slice = new Slice(None, Some(int), None)

    /** Creates a slice which drops the first `n` elements of the array. Alias
      * for [[start]].
      * @param n
      *   The number of elements to drop.
      * @return
      *   The slice.
      * @see
      *   [[start]]
      */
    def drop(n: Int): Slice = start(n)

    /** Creates a slice which takes the first `n` elements of the array. Alias
      * for [[end]].
      * @param n
      *   The number of elements to take.
      * @return
      *   The slice.
      * @see
      *   [[end]]
      */
    def take(n: Int): Slice = end(n)

    /** Creates a slice which takes the rightmost `n` elements of the array.
      * @param n
      *   The number of elements to take.
      * @return
      *   The slice.
      */
    def takeRight(n: Int): Slice = start(0 - n)

    /** Creates a slice which drops the rightmost `n` elements of the array.
      * @param n
      *   The number of elements to drop.
      * @return
      *   The slice.
      */
    def dropRight(n: Int): Slice = end(0 - n)

    /** Creates a slice with the given step, ie, takes every `n`th element.
      * @param step
      *   The step of the slice.
      * @return
      *   The slice.
      */
    def everyN(step: Int): Slice = new Slice(None, None, Some(step))

    /** Creates a slice with the given start and end.
      * @param start
      *   The starting right of the slice, inclusive.
      * @param end
      *   The ending right of the slice, exclusive.
      * @return
      *   The slice.
      */
    def apply(start: Int, end: Int): Slice =
      new Slice(Some(start), Some(end), None)

    /** Creates a slice with the given start, end, and step.
      * @param start
      *   The starting right of the slice, inclusive.
      * @param end
      *   The ending right of the slice, exclusive.
      * @param step
      *   The step of the slice.
      * @return
      *   The slice.
      */
    def apply(start: Int, end: Int, step: Int): Slice =
      new Slice(Some(start), Some(end), Some(step))

    /** Constructs a slice from the given start, end, and step if defined. If if
      * none of the parameters are defined, then [[scala.None]] will be
      * returned, otherwise the slice will be returned.
      * @param start
      *   The starting right of the slice, inclusive.
      * @param end
      *   The ending right of the slice, exclusive.
      * @param step
      *   The step of the slice.
      * @return
      *   The slice, if valid.
      */
    def apply(
        start: Option[Int],
        end: Option[Int],
        step: Option[Int]
    ): Option[Slice] =
      Option.when(start.isDefined || end.isDefined || step.isDefined) {
        new Slice(start, end, step)
      }
  }

  sealed trait ScriptSelector extends Selector

  final case class Filter(expression: Expression) extends ScriptSelector {

    override def toString: String = s"?($expression)"
  }

  final case class Script(expression: Expression) extends ScriptSelector {

    override def toString: String = s"($expression)"
  }
}
