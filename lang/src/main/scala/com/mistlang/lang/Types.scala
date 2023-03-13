package com.mistlang.lang

sealed trait Type

object Types {
  case object AnyType extends Type

  case object IntType extends Type

  case object StrType extends Type

  case object BoolType extends Type

  case object UnitType extends Type

  case class FuncType(args: List[Type], out: Type) extends Type
}
