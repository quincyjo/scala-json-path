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
import com.quincyjo.jsonpath.JsonPath.SingularSelector.SingularSelectorWrapper
import com.quincyjo.jsonpath.JsonPath._
import com.quincyjo.jsonpath.parser.util.StringEscapes
// import scala.annotation.{tailrec, targetName}

sealed trait JsonPath extends Serializable {

  def root: JsonPathRoot

  def segments: List[JsonPathSegment]

  /** Returns true if this path is absolute, IE; if it has is rooted in the root
    * document.
    * @return
    *   A boolean indicating if this path is absolute.
    */
  def isAbsolute: Boolean =
    root == Root

  /** Returns true if this path is relative, IE; if it is rooted in the current
    * document.
    * @return
    *   A boolean indicating if this path is dynamic.
    */
  def isRelative: Boolean =
    root == Current

  /** Appends the given [[JsonPath.JsonPathSegment]] to this path.
    *
    * @param that
    *   The [[JsonPath.JsonPathSegment]] to append.
    * @return
    *   This path with the given node appended.
    */
  def appended(that: JsonPathSegment): JsonPath

  /** Appends all of the given [[JsonPath.JsonPathSegment]] s to this path.
    *
    * @param that
    *   An iterable of the [[JsonPath.JsonPathSegment]] s to append.
    * @return
    *   This path with the given nodes appended.
    */
  def appendedAll(that: Iterable[JsonPathSegment]): JsonPath

  /** Prepends the given [[JsonPath.JsonPathSegment]] to this path.
    *
    * @param that
    *   The [[JsonPath.JsonPathSegment]] to prepend.
    * @return
    *   This path with the given node prepended.
    */
  def prepended(that: JsonPathSegment): JsonPath

  /** Prepends all the given [[JsonPath.JsonPathSegment]] s to this path.
    *
    * @param that
    *   An iterable of the [[JsonPath.JsonPathSegment]] s to prepend.
    * @return
    *   This path with the given nodes prepended.
    */
  def prependedAll(that: Iterable[JsonPathSegment]): JsonPath

  /** DSL to append a child [[JsonPath.Selector]] to this [[JsonPath]].
    *
    * @param singleSelectorWrapper
    *   A wrapped [[JsonPath.SingularSelector]] to append.
    * @return
    *   This path with the given selector appended.
    */
  // @targetName("select")
  def /(singleSelectorWrapper: SingularSelectorWrapper): JsonPath =
    /(singleSelectorWrapper.value)

  def /(selector: Selector): JsonPath = selector match {
    case selector: SingularSelector => /(selector)
    case selector: MultiSelector    => /(selector)
  }

  /** DSL to append a child [[JsonPath.Selector]] to this [[JsonPath]].
    * @param selector
    *   The [[JsonPath.Selector]] to append.
    * @return
    *   This path with the given selector appended.
    */
  // @targetName("select")
  def /(selector: SingularSelector): JsonPath

  def /(selector: MultiSelector): Query =
    Query(root, segments appended Children(selector))

  /** DSL to append a recursive descent [[JsonPath.Selector]] to this
    * [[JsonPath]].
    * @param selector
    *   The [[JsonPath.Selector]] to append.
    * @return
    *   This path with the given selector applied recursively.
    */
  def */(selector: Selector): Query =
    Query(root, segments appended RecursiveDescent(selector))

  def */(selector: SingularSelectorWrapper): Query =
    */(selector.value)

  /** DSL to append a filter [[JsonPath.Selector]] to this [[JsonPath]].
    * @param expression
    *   The [[Expression]] to filter by.
    * @return
    *   This path with the given filter applied.
    */
  def /?(expression: Expression.LogicalType): Query =
    Query(root, segments appended Children(Filter(expression)))

  /** Takes the first `n` nodes of this path.
    *
    * @param n
    *   The number of nodes to take.
    * @return
    *   This path truncated to the first `n` [[JsonPath.JsonPathSegment]] s.
    */
  def take(n: Int): JsonPath

  /** Drops the first `n` nodes of this path.
    *
    * @param n
    *   The number of nodes to drop.
    * @return
    *   This path with the first `n` [[JsonPath.JsonPathSegment]] s dropped.
    */
  def drop(n: Int): JsonPath

  /** Takes the last `n` nodes of this path.
    *
    * @param n
    *   The number of nodes to take.
    * @return
    *   This path truncated to the last `n` [[JsonPath.JsonPathSegment]] s.
    */
  def takeRight(n: Int): JsonPath

  /** Drops the last `n` nodes of this path.
    *
    * @param n
    *   The number of nodes to drop.
    * @return
    *   This path with the last `n` [[JsonPath.JsonPathSegment]] s dropped.
    */
  def dropRight(n: Int): JsonPath

  /** Returns true if this path is empty and false otherwise. The root node of
    * the path is not considered.
    * @return
    *   True if this path is empty, false otherwise.
    */
  def isEmpty: Boolean = segments.isEmpty

  /** Returns true if this path is not empty and false otherwise. The root node
    * of the path is not considered.
    * @return
    *   True if this path is not empty, false otherwise.
    */
  def nonEmpty: Boolean = segments.nonEmpty

  /** Returns the number of [[JsonPath.JsonPathSegment]] in this path.
    *
    * @return
    *   The number of [[JsonPath.JsonPathSegment]] in this path.
    */
  def size: Int = segments.size

  /** Returns the length of this path.
    * @return
    *   The length of this path.
    */
  def length: Int = segments.length

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
  def parent: JsonPath

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
  def resolve(
      that: JsonPath
  ): JsonPath =
    if (that.isAbsolute) that
    else
      this -> that match {
        case (a: SingularQuery, b: SingularQuery) =>
          SingularQuery(root, segments = a.segments appendedAll b.segments)
        case a -> b =>
          Query(root, segments = a.segments appendedAll b.segments)
      }

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
  def resolveSibling(
      that: JsonPath
  ): JsonPath =
    if (!hasParent) that
    else parent.resolve(that)

  /** Returns this path as an absolute path, IE, with a [[root]] of
    * [[JsonPath.JsonPathRoot.Root]].
    * @return
    *   This path as an absolute path.
    */
  def toAbsolutePath: JsonPath

  /** Returns this path as a relative path, ie with no [[root]] of
    * [[JsonPath.JsonPathRoot.Current]].
    * @return
    *   This path as a relative path.
    */
  def toRelativePath: JsonPath

  override def toString: String =
    s"$root${segments.mkString}"
}

object JsonPath {

  final val absolute: SingularQuery =
    SingularQuery(Root, Nil)

  final val relative: SingularQuery =
    SingularQuery(Current, Nil)

  def $ : SingularQuery = absolute
  def `@` : SingularQuery = relative

  def apply(
      root: JsonPathRoot,
      segment: Child,
      segments: Child*
  ): SingularQuery =
    SingularQuery(root, segments.toList.prepended(segment))

  def apply(
      root: JsonPathRoot,
      segment: JsonPathSegment,
      segments: JsonPathSegment*
  ): JsonPath =
    apply(root, segments prepended segment)

  def apply(root: JsonPathRoot, segments: Iterable[JsonPathSegment]): JsonPath =
    if (segments.forall(_.isSingular)) {
      SingularQuery(
        root,
        segments.toList.asInstanceOf[List[Child]]
      )
    } else Query(root, segments.toList)

  /** A [[JsonPath]] that is singular query. This is defined as being composed
    * semantically in such a way that it cannot resolve to more than a single
    * node. Specifically, this containing only name and index selectors.
    * @param root
    *   The root of the path.
    * @param segments
    *   The segments of the path.
    */
  final case class SingularQuery(
      root: JsonPathRoot,
      segments: List[Child]
  ) extends JsonPath {

    /** Appends the given [[JsonPath.Child]] to this path explicitly maintaining
      * a singular query.
      *
      * @param child
      *   The [[JsonPath.JsonPathSegment]] to append.
      * @return
      *   This path with the given node appended.
      */
    def appended(child: Child): SingularQuery =
      copy(segments = segments appended child)

    override def appended(segment: JsonPathSegment): JsonPath =
      segment match {
        case segment: Child => appended(segment)
        case segment        => Query(root, segments appended segment)
      }

    override def appendedAll(that: Iterable[JsonPathSegment]): JsonPath =
      JsonPath(root, segments appendedAll that)

    /** Appends all the given [[JsonPath.Child]] to this path, explicitly
      * maintaining a singular query.
      * @param that
      *   The singular selectors to append.
      * @return
      *   This path with the given nodes appended.
      */
    def appendedAll(that: Iterable[Child]): SingularQuery =
      copy(segments = segments appendedAll that)

    /** Prepends the given [[JsonPath.Child]] to this path, explicitly
      * maintaining a singular query.
      *
      * @param that
      *   The singular selector to prepend.
      * @return
      *   This path with the given node prepended.
      */
    def prepended(that: Child): SingularQuery =
      copy(segments = segments.prepended(that))

    override def prepended(that: JsonPathSegment): JsonPath = that match {
      case child: Child => prepended(child)
      case segment      => Query(root, segments.prepended(segment))
    }

    /** Prepends all the given [[JsonPath.Child]] to this path, explicitly
      * maintaining a singular query.
      *
      * @param that
      *   The singular selectors to prepend.
      * @return
      *   This path with the given nodes prepended.
      */
    def prependedAll(that: Iterable[Child]): SingularQuery =
      copy(segments = segments prependedAll that)

    override def prependedAll(that: Iterable[JsonPathSegment]): JsonPath =
      JsonPath(root, segments prependedAll that)

    override def /(
        singleSelectorWrapper: SingularSelectorWrapper
    ): SingularQuery =
      appended(Child(singleSelectorWrapper.value))

    def /(selector: SingularSelector): SingularQuery =
      appended(Child(selector))

    override def take(n: Int): SingularQuery =
      copy(segments = segments.take(n))

    override def drop(n: Int): SingularQuery =
      copy(segments = segments.drop(n))

    override def takeRight(n: Int): SingularQuery =
      copy(segments = segments.takeRight(n))

    override def dropRight(n: Int): SingularQuery =
      copy(segments = segments.dropRight(n))

    override def parent: SingularQuery =
      copy(segments = segments.dropRight(1))

    override def toAbsolutePath: SingularQuery =
      if (isAbsolute) this
      else copy(Root)

    /** Returns this path as a relative path, ie with no [[root]] of
      * [[JsonPath.JsonPathRoot.Current]].
      *
      * @return
      *   This path as a relative path.
      */
    override def toRelativePath: SingularQuery =
      if (isRelative) this
      else copy(Current)
  }

  /** A [[JsonPath]] that is not singular query. This cannot be built explicitly
    * but rather is shifted to when a singular query is given a segment which is
    * not itself singular.
    * @param root
    *   The root of the path.
    * @param segments
    *   The segments of the path.
    */
  final case class Query private[JsonPath] (
      root: JsonPathRoot,
      segments: List[JsonPathSegment]
  ) extends JsonPath {

    override def appended(segment: JsonPathSegment): Query =
      copy(segments = segments appended segment)

    override def prepended(that: JsonPathSegment): JsonPath =
      copy(segments = segments prepended that)

    override def appendedAll(that: Iterable[JsonPathSegment]): JsonPath =
      copy(segments = segments appendedAll that)

    override def prependedAll(that: Iterable[JsonPathSegment]): JsonPath =
      copy(segments = segments prependedAll that)

    def /(selector: SingularSelector): Query =
      copy(segments = segments appended Child(selector))

    override def take(n: Int): JsonPath =
      JsonPath(root, segments.take(n))

    override def drop(n: Int): JsonPath =
      JsonPath(root, segments.drop(n))

    override def takeRight(n: Int): JsonPath =
      JsonPath(root, segments.takeRight(n))

    override def dropRight(n: Int): JsonPath =
      JsonPath(root, segments.dropRight(n))

    override def parent: JsonPath =
      JsonPath(root, segments.dropRight(1))

    override def toAbsolutePath: Query =
      if (isAbsolute) this
      else copy(Root)

    override def toRelativePath: Query =
      if (isRelative) this
      else copy(Current)
  }

  sealed trait JsonPathSegment {

    def isSingular: Boolean

    def selector: Selector
  }

  object JsonPathSegment {

    def apply(selector: Selector): JsonPathSegment = selector match {
      case selector: SingularSelector => Child(selector)
      case selector: MultiSelector    => Children(selector)
    }
  }

  sealed trait JsonPathRoot

  object JsonPathRoot {

    case object Root extends JsonPathRoot {
      override def toString: String = "$"
    }

    case object Current extends JsonPathRoot {
      override def toString: String = "@"
    }
  }

  /** Recursively descends through a JSON, exposing all associative JSON nodes
    * throughout the target right. The provided selector is then applied to each
    * JSON discovered by the recursive descent.
    *
    * @param selector
    *   A selector to apply to the recursive descent.
    */
  final case class RecursiveDescent(selector: Selector)
      extends JsonPathSegment {

    override val isSingular: Boolean = false

    override def toString: String = selector match {
      case Wildcard => s"..$Wildcard"
      case selector => s"..[$selector]"
    }
  }

  object RecursiveDescent {

    def apply(selector: SingularSelectorWrapper): RecursiveDescent =
      apply(selector.value)
  }

  /** [[JsonPathSegment]] that applies a [[Selector]] to a JSON to match zero or
    * more of its leaf nodes or children.
    *
    * @param selector
    *   The [[SingularSelector]] which this node contains.
    */
  final case class Child(selector: SingularSelector) extends JsonPathSegment {

    override val isSingular: Boolean = true

    override def toString: String = s"[$selector]"
  }

  object Child {

    def apply(selector: SingularSelectorWrapper): Child =
      new Child(selector.value)

    def attribute(attribute: String): Child =
      new Child(Attribute(attribute))

    def index(index: Int): Child =
      new Child(Index(index))
  }

  final case class Children(selector: MultiSelector) extends JsonPathSegment {

    override val isSingular: Boolean = false

    override def toString: String = s"[$selector]"
  }

  object Children {

    def apply(
        selector: SingularSelectorWrapper,
        selector2: SingularSelectorWrapper,
        selectors: SingularSelectorWrapper*
    ): Children =
      new Children(Union(selector, selector2, selectors *))
  }

  /** A description of a selection of properties of a JSON value, such as an
    * index, attribute, slice, etc.
    */
  sealed trait Selector

  /** Describes a selector that is not composed of multiple selectors. That is,
    * all selectors except for [[Union]].
    */
  sealed trait ComposableSelector extends Selector {

    /** Creates a union between this single selector and another.
      * @param that
      *   The second selector.
      * @return
      *   A union of this selector and the other.
      */
    def and(that: SingularSelector): Union = Union(this, that)
  }

  /** A selector which describes a single property selection.
    */
  sealed trait SingularSelector extends ComposableSelector

  object SingularSelector {

    sealed trait SingularSelectorWrapper {

      def value: SingularSelector
    }

    private case class SingularSelectorWrapperImpl(field: SingularSelector)
        extends SingularSelectorWrapper {

      override def value: SingularSelector = field
    }

    implicit def toSingularSelectorWrapper[T](field: T)(implicit
        w: SingularSelectorMagnet[T]
    ): SingularSelectorWrapper =
      SingularSelectorWrapperImpl(w(field))

    sealed trait SingularSelectorMagnet[T] {

      def apply(t: T): SingularSelector
    }

    object SingularSelectorMagnet {

      implicit case object AttributeMagnet
          extends SingularSelectorMagnet[String] {

        override def apply(t: String): SingularSelector = Attribute(t)
      }

      implicit case object IndexMagnet extends SingularSelectorMagnet[Int] {

        override def apply(t: Int): SingularSelector = Index(t)
      }
    }
  }

  sealed trait MultiSelector extends Selector

  /** Selects the given attribute by name from a JSON object.
    *
    * @param value
    *   The attribute name to select.
    */
  final case class Attribute(value: String) extends SingularSelector {

    /** Returns true if the name is a simple identifier, meaning that it only
      * contains letters and digits.
      * @return
      *   True if the name is a simple identifier or false otherwise.
      */
    def isSimple: Boolean =
      value.headOption.forall(_.isLetter) && value.forall(_.isLetterOrDigit)

    override def toString: String =
      s"'${StringEscapes.escapeSingleQuotes(value)}'"
  }

  /** Selects the given index from a JSON array.
    * @param value
    *   The index to select.
    */
  final case class Index(value: Int) extends SingularSelector {

    override def toString: String = value.toString
  }

  /** Selects all direct children of an associative JSON. If the target is
    * atomic, nothing is matched.
    */
  case object Wildcard extends ComposableSelector with MultiSelector {

    override def toString: String = "*"
  }

  /** Selects the union of two or more [[SingularSelector]] s, ie, this
    * [[Selector]] selects values which match any of the given
    * [[SingularSelector]] s.
    *
    * @param head
    *   The first [[SingularSelector]] in the union.
    * @param second
    *   The second [[SingularSelector]] in the union.
    * @param tail
    *   The remaining [[SingularSelector]] s in the union.
    */
  final case class Union(
      head: ComposableSelector,
      second: ComposableSelector,
      tail: Seq[ComposableSelector] = Seq.empty
  ) extends MultiSelector {

    def and(that: ComposableSelector): Union =
      appended(that)

    def appended(that: ComposableSelector): Union =
      copy(tail = tail.appended(that))

    // @targetName("add")
    def +(that: ComposableSelector): Union =
      appended(that)

    def toSet: Set[ComposableSelector] = Set(head, second) ++ tail

    override def toString: String =
      tail
        .prepended(second)
        .prepended(head)
        .mkString(",")

    override def equals(obj: Any): Boolean = obj match {
      case that: Union =>
        toSet == that.toSet
      case _ => false
    }
  }

  object Union {

    def apply(
        head: SingularSelectorWrapper,
        second: SingularSelectorWrapper,
        tail: SingularSelectorWrapper*
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
  ) extends ComposableSelector
      with MultiSelector {

    override def toString: String =
      Seq(
        start.fold("")(_.toString),
        end.fold("")(_.toString)
      ).mkString(":") + step.fold("")(step => s":$step")
  }

  object Slice {

    /** Creates a slice with the given start index, ie, the subarray from the
      * given index`.
      * @param int
      *   The start of the slice.
      * @return
      *   The slice.
      */
    def start(int: Int): Slice = new Slice(Some(int), None, None)

    /** Creates a slice with the given end index, ie, the subarray up to the
      * given index.
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

    /** Creates a slice which takes the right most `n` elements of the array.
      * @param n
      *   The number of elements to take.
      * @return
      *   The slice.
      */
    def takeRight(n: Int): Slice = start(0 - n)

    /** Creates a slice which drops the right most `n` elements of the array.
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
      *   The starting index of the slice, inclusive.
      * @param end
      *   The ending index of the slice, exclusive.
      * @return
      *   The slice.
      */
    def apply(start: Int, end: Int): Slice =
      new Slice(Some(start), Some(end), None)

    /** Creates a slice with the given start, end, and step.
      * @param start
      *   The starting index of the slice, inclusive.
      * @param end
      *   The ending index of the slice, exclusive.
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
      *   The starting index of the slice, inclusive.
      * @param end
      *   The ending index of the slice, exclusive.
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

  final case class Filter(expression: Expression.LogicalType)
      extends MultiSelector
      with ComposableSelector {

    override def toString: String = s"?($expression)"
  }
}
