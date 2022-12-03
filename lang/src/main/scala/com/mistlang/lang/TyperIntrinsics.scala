package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types._
import com.mistlang.lang.RuntimeValue.{Func, Type}

object TyperIntrinsics {
  val intrinsics: Map[String, RuntimeValue] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyType, AnyType, BoolType),
    "Unit" -> UnitTypeInstance,
    "Any" -> AnyTypeInstance,
    "Int" -> IntTypeInstance,
    "String" -> StrTypeInstance,
    "Func" -> Func { l =>
      Typer.assert(l.nonEmpty, "Function type must declare an output type")
      val types = l.map(_.getType)
      val args = types.take(l.length - 1)
      BasicFuncTypeInstance(args, types.last)
    },
    "Tuple" -> Type(TypelevelFunc { l => Type(TupleType(l)) }),
    "at" -> Type(At)
  )
}
