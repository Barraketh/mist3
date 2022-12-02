package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.{Dict, Func}
import com.mistlang.lang.Types.BasicFuncType.op
import com.mistlang.lang.Types._

object TyperIntrinsics {
  val intrinsics: Map[String, Dict] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyType, AnyType, BoolType),
    "get" -> op(DictType, StrType, AnyType),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType
  )
}
