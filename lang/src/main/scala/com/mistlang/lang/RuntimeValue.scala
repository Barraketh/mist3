package com.mistlang.lang

sealed trait RuntimeValue
object RuntimeValue {
  sealed trait Primitive extends RuntimeValue
  case class StrVal(value: String) extends Primitive
  case class BoolVal(value: Boolean) extends Primitive
  case class IntVal(value: Int) extends Primitive
  case object UnitVal extends Primitive
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue

  sealed trait Type extends RuntimeValue

  object Types {
    case object AnyType extends Type
    case object IntType extends Type
    case object StrType extends Type
    case object BoolType extends Type
    case object UnitType extends Type
    case class FuncType(args: List[Type], out: Type) extends Type
  }
}
