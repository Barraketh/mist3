package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object RuntimeIntrinsics {

  val intrinsics: Map[String, RuntimeValue] = Map(
    "+" -> Func { case IntVal(a) :: IntVal(b) :: Nil => IntVal(a + b) },
    "-" -> Func { case IntVal(a) :: IntVal(b) :: Nil => IntVal(a - b) },
    "*" -> Func { case IntVal(a) :: IntVal(b) :: Nil => IntVal(a * b) },
    "==" -> Func { case (a: Primitive) :: (b: Primitive) :: Nil => BoolVal(a == b) },
    "Unit" -> UnitVal,
    "Null" -> NullVal
  )
}
