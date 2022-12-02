package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types.BasicFuncType.op
import com.mistlang.lang.RuntimeValue.Types._

object TyperIntrinsics {
  val intrinsics: Map[String, Type] = Map(
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
