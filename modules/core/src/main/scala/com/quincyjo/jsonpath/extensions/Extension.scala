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

/** An extension to provide a function extension to a
  * [[com.quincyjo.jsonpath.parser.JsonPathParser]]. This allows a parser to
  * safely parse and validate a call its function extension.
  *
  * @param name
  *   The name of the extension function
  * @param cons
  *   The constructor for the extension function
  * @param parseArgs
  *   The arguments parser for the extension function
  */
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

    implicit def argsToTuple5[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 5 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple6[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 6 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple7[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6, P7)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 7 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple8[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6, P7, P8)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 8 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple9[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6, P7, P8, P9)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 9 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple10[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 10 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple11[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 11 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple12[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 12 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple13[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 13 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple14[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 14 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple15[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 15 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple16[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16)
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 16 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple17[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 17 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple18[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible,
        P18 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17,
          P18
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17),
          validate[P18](p18)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 18 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple19[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible,
        P18 <: Expression: Expression.Coercible,
        P19 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17,
          P18,
          P19
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: p19 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17),
          validate[P18](p18),
          validate[P19](p19)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 19 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple20[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible,
        P18 <: Expression: Expression.Coercible,
        P19 <: Expression: Expression.Coercible,
        P20 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17,
          P18,
          P19,
          P20
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: p19 :: p20 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17),
          validate[P18](p18),
          validate[P19](p19),
          validate[P20](p20)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 20 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple21[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible,
        P18 <: Expression: Expression.Coercible,
        P19 <: Expression: Expression.Coercible,
        P20 <: Expression: Expression.Coercible,
        P21 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17,
          P18,
          P19,
          P20,
          P21
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: p19 :: p20 :: p21 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17),
          validate[P18](p18),
          validate[P19](p19),
          validate[P20](p20),
          validate[P21](p21)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 21 arguments, but got ${other.size}")
        )
    }

    implicit def argsToTuple22[
        P1 <: Expression: Expression.Coercible,
        P2 <: Expression: Expression.Coercible,
        P3 <: Expression: Expression.Coercible,
        P4 <: Expression: Expression.Coercible,
        P5 <: Expression: Expression.Coercible,
        P6 <: Expression: Expression.Coercible,
        P7 <: Expression: Expression.Coercible,
        P8 <: Expression: Expression.Coercible,
        P9 <: Expression: Expression.Coercible,
        P10 <: Expression: Expression.Coercible,
        P11 <: Expression: Expression.Coercible,
        P12 <: Expression: Expression.Coercible,
        P13 <: Expression: Expression.Coercible,
        P14 <: Expression: Expression.Coercible,
        P15 <: Expression: Expression.Coercible,
        P16 <: Expression: Expression.Coercible,
        P17 <: Expression: Expression.Coercible,
        P18 <: Expression: Expression.Coercible,
        P19 <: Expression: Expression.Coercible,
        P20 <: Expression: Expression.Coercible,
        P21 <: Expression: Expression.Coercible,
        P22 <: Expression: Expression.Coercible
    ]: ExtensionFunctionArgs[
      (
          P1,
          P2,
          P3,
          P4,
          P5,
          P6,
          P7,
          P8,
          P9,
          P10,
          P11,
          P12,
          P13,
          P14,
          P15,
          P16,
          P17,
          P18,
          P19,
          P20,
          P21,
          P22
      )
    ] = {
      case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: p12 :: p13 :: p14 :: p15 :: p16 :: p17 :: p18 :: p19 :: p20 :: p21 :: p22 :: Nil =>
        (
          validate[P1](p1),
          validate[P2](p2),
          validate[P3](p3),
          validate[P4](p4),
          validate[P5](p5),
          validate[P6](p6),
          validate[P7](p7),
          validate[P8](p8),
          validate[P9](p9),
          validate[P10](p10),
          validate[P11](p11),
          validate[P12](p12),
          validate[P13](p13),
          validate[P14](p14),
          validate[P15](p15),
          validate[P16](p16),
          validate[P17](p17),
          validate[P18](p18),
          validate[P19](p19),
          validate[P20](p20),
          validate[P21](p21),
          validate[P22](p22)
        ).tupled
      case other =>
        Validated.invalidNel(
          MissingArg(s"expects 22 arguments, but got ${other.size}")
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
