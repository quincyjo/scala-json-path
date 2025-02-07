package com.quincyjo.jsonpath

import com.quincyjo.jsonpath.Expression.ExtensionType

sealed trait ExtensionFunction[Parameters, Result <: ExtensionType]
    extends Expression.Extension {

  def apply(parameters: Parameters): Result

  def name: String
}

object ExtensionFunction {

  trait ExtensionFunction1[
      Parameters <: ExtensionType,
      Result <: ExtensionType
  ] extends ExtensionFunction[Parameters, Result]

  trait ExtensionFunction2[
      Parameters <: (ExtensionType, ExtensionType),
      Result <: ExtensionType
  ] extends ExtensionFunction[Parameters, Result]

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
