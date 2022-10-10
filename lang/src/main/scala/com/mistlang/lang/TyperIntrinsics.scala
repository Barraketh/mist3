package com.mistlang.lang

import com.mistlang.lang.Type.{AnyType, BoolType, FuncType, IntType, StrType}

object TyperIntrinsics {
  val intrinsics: Map[String, Type] = Map(
    "Int" -> IntType,
    "String" -> StrType,
    "Boolean" -> BoolType,
    "Any" -> AnyType,
    "+" -> FuncType(List(Type.Arg("a", IntType), Type.Arg("b", IntType)), IntType),
    "-" -> FuncType(List(Type.Arg("a", IntType), Type.Arg("b", IntType)), IntType),
    "==" -> FuncType(List(Type.Arg("a", AnyType), Type.Arg("b", AnyType)), BoolType)
  )
}
