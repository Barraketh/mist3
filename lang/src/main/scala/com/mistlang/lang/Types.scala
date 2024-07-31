package com.mistlang.lang

sealed trait Comptime

sealed trait Type extends Comptime

object Types {
  case object AnyType extends Type
  case object IntType extends Type
  case object StrType extends Type
  case object BoolType extends Type
  case object UnitType extends Type
  case object NullType extends Type
  case class FuncType(args: List[Type], out: Type) extends Type
  case class StructType(name: String, namespace: String, args: List[(String, Type)]) extends Type
  case class NamespaceType(children: Map[String, Type]) extends Type
}

sealed trait ComptimeValue extends Comptime
object ComptimeValue {
  case class IntValue(i: Int) extends ComptimeValue
  case class BoolValue(b: Boolean) extends ComptimeValue
  case class StringValue(s: String) extends ComptimeValue

}
