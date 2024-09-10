package com.mistlang.lang

sealed trait ComptimeValue

object ComptimeValue {
  case class SimpleValue(value: Any) extends ComptimeValue
  case class Dict(m: Map[String, ComptimeValue]) extends ComptimeValue
  case class Func(f: List[TypedValue] => TypedValue) extends ComptimeValue
  case object UnitValue extends ComptimeValue
}

sealed trait Type extends ComptimeValue

object Types {
  case object AnyType extends Type
  case object IntType extends Type
  case object StrType extends Type
  case object BoolType extends Type
  case object UnitType extends Type
  case object TypeType extends Type
  case class ArrayType(underlying: Type) extends Type
  case class FuncType(args: List[Type], out: Type, isStar: Boolean = false) extends Type
  case class StructType(args: List[(String, Type)]) extends Type
}

case class TypedValue(tpe: Type, value: Option[ComptimeValue], name: Option[String] = None)
