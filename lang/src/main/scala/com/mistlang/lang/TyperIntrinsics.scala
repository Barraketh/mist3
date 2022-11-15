package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.BasicFuncType.op
import com.mistlang.lang.RuntimeValue._

object TyperIntrinsics {
  val intrinsics: Map[String, RuntimeValue.Type] = Map(
    "+" -> op(IntType, IntType, IntType),
    "-" -> op(IntType, IntType, IntType),
    "*" -> op(IntType, IntType, IntType),
    "==" -> op(AnyType, AnyType, BoolType),
    "if" -> TypelevelFunc({ case BoolType :: BasicFuncType(Nil, successTpe) :: BasicFuncType(Nil, failTpe) :: Nil =>
      if (successTpe == failTpe) successTpe
      else AnyType
    }),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType
  )
}
