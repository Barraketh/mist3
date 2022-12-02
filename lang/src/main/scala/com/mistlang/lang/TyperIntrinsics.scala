package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Func
import com.mistlang.lang.RuntimeValue.Types.BasicFuncType.op
import com.mistlang.lang.RuntimeValue.Types._

object TyperIntrinsics {
  val intrinsics: Map[String, RuntimeValue] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyType, AnyType, BoolType),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType,
    "Func" -> Func { l =>
      Typer.assert(l.nonEmpty, "Function type must declare an output type")
      val types = l.map(_.getType)
      val args = types.take(l.length - 1)
      BasicFuncType(args, types.last)
    }
  )
}
