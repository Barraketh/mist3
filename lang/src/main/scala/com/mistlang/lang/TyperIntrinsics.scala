package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Dict
import com.mistlang.lang.Types.BasicFuncType.op
import com.mistlang.lang.Types._

object TyperIntrinsics {
  val intrinsics: Map[String, Dict] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyPrimitive, AnyPrimitive, BoolType),
    "get" -> BasicFuncType(List(("dict", AnyPrimitive), ("key", StrType)), AnyPrimitive, isLambda = false),
    "Unit" -> UnitType,
    "Any" -> AnyPrimitive,
    "Int" -> IntType,
    "String" -> StrType
  )
}
