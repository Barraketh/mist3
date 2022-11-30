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
    "get" -> TypeFunc(Func(l => {
      Typer.assert(l.length == 2, s"Unexpected number of params - expected 2, got ${l.length}")
      Typer.validateType(RecordType, l.head, "dict")
      Typer.validateType(StrType, l(1), "key")
      val fields = l.head.getDict("fields").getDict
      val keyVal = l(1).getDict("value").getString
      fields(keyVal)
    })),
    "Unit" -> UnitType,
    "Any" -> AnyType,
    "Int" -> IntType,
    "String" -> StrType
  )
}
