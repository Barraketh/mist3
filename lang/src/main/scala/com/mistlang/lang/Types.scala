package com.mistlang.lang

sealed trait InterpreterValue

object InterpreterValue {
  case class PrimitiveValue(value: Any) extends InterpreterValue
  case class Dict(m: Map[String, InterpreterValue]) extends InterpreterValue
  case class Func(f: List[InterpreterValue] => InterpreterValue) extends InterpreterValue
  case object UnitValue extends InterpreterValue

}

sealed trait ComptimeValue

object ComptimeValue {
  case class PrimitiveValue(value: Any) extends ComptimeValue
  case class Dict(m: Map[String, ComptimeValue]) extends ComptimeValue
  case class Func(f: List[TypedValue] => TypedValue) extends ComptimeValue
}

sealed trait Type extends ComptimeValue

object Types {
  case object AnyType extends Type
  case object IntType extends Type
  case object StrType extends Type
  case object BoolType extends Type
  case object UnitType extends Type
  case object NullType extends Type
  case object TypeType extends Type
  case class FuncType(args: List[Type], out: Type, isStar: Boolean = false) extends Type
  case class StructType(name: String, namespace: String, args: List[(String, Type)]) extends Type
  case class NamespaceType(children: Map[String, Type]) extends Type
}

case class TypedValue(tpe: Type, value: Option[ComptimeValue])
