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

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import com.quincyjo.jsonpath.Expression
import com.quincyjo.jsonpath.extensions.Extension.InvalidArgs.MissingArg
import com.quincyjo.jsonpath.extensions.Extension.{
  ExtensionFunctionArgs,
  InvalidArgs
}
import com.quincyjo.jsonpath.parser.models.ValueAt

final case class Extension[
    ArgsType,
    ExtensionType <: FunctionExtension[ArgsType]
](name: String)(cons: ArgsType => ExtensionType & Expression)(implicit
    parseArgs: ExtensionFunctionArgs[ArgsType]
) extends PartialFunction[
      String,
      List[ValueAt[Expression]] => ValidatedNel[InvalidArgs, FunctionExtension[
        ?
      ] & Expression]
    ] {

  private val parse: PartialFunction[
    String,
    List[ValueAt[Expression]] => ValidatedNel[
      InvalidArgs,
      ExtensionType & Expression
    ]
  ] = {
    case `name` => { args =>
      parseArgs(args).map(cons)
    }
  }

  override def isDefinedAt(x: String): Boolean = parse.isDefinedAt(x)

  override def apply(
      x: String
  ): List[ValueAt[Expression]] => ValidatedNel[
    InvalidArgs,
    ExtensionType & Expression
  ] =
    parse(x)
}

object Extension {

  sealed trait InvalidArgs {

    def message: String
  }

  object InvalidArgs {

    final case class InvalidArg(arg: ValueAt[Expression], message: String)
        extends InvalidArgs

    final case class MissingArg(message: String) extends InvalidArgs
  }

  trait ExtensionFunctionArgs[Args] {

    def apply(args: List[ValueAt[Expression]]): ValidatedNel[InvalidArgs, Args]
  }

  object ExtensionFunctionArgs {

    // TODO: This can probably be done against case classes by type with Shapeless.
    implicit def argsToSingle[P1 <: Expression: Expression.Coercible]
        : ExtensionFunctionArgs[P1] = {
      case single :: Nil =>
        validate[P1](single)
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 1 argument, but got ${other.size}")
        )
    }

    implicit def argsToTuple2[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2)] = {
      case p1 :: p2 :: Nil =>
        (validate[P1](p1), validate[P2](p2)).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 2 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple3[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3)] = {
      case p1 :: p2 :: p3 :: Nil =>
        (validate[P1](p1), validate[P2](p2), validate[P3](p3)).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 3 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple4[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4)] = {
      case p1 :: p2 :: p3 :: p4 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 4 arguments, but got ${other.size}")
        )
    }

    private def validate[T <: Expression: Expression.Coercible](
        expression: ValueAt[Expression]
    ): ValidatedNel[InvalidArgs, T] =
      implicitly[Expression.Coercible[T]]
        .coerce(expression.value)
        .leftMap { msg =>
          InvalidArgs.InvalidArg(expression, msg)
        }
        .toValidatedNel
  }
}
