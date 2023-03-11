package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types._

object TyperIntrinsics {
  val intrinsics: Map[String, RuntimeValue] = Map(
    "+" -> FuncType(List(IntType, IntType), IntType),
    "-" -> FuncType(List(IntType, IntType), IntType),
    "*" -> FuncType(List(IntType, IntType), IntType),
    "==" -> FuncType(List(AnyType, AnyType), BoolType),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType
  )
}
