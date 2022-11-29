package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Type.BasicFuncType.op
import com.mistlang.lang.RuntimeValue.Type._

object TyperIntrinsics {
  val intrinsics: Map[String, RuntimeValue.Type] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyType, AnyType, BoolType),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType
  )
}
