package com.mistlang.lang

import com.mistlang.lang.Type.{AnyType, Arg, BasicFuncType, BoolType, IntType, StrType, TypelevelFunc, UnitType}

sealed trait RuntimeValue

object RuntimeValue {
  case class FuncVal(numArgs: Option[Int], f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue
  case object UnitVal extends RuntimeValue
  case class IntVal(value: Int) extends RuntimeValue
  case class BoolVal(value: Boolean) extends RuntimeValue
  case class StrVal(value: String) extends RuntimeValue
  case class TupleVal(arr: List[RuntimeValue]) extends RuntimeValue
}

case class TaggedType(t: Type, tags: Map[String, RuntimeValue]) extends RuntimeValue
object TaggedType {
  def apply(t: Type, tags: (String, RuntimeValue)*): TaggedType = {
    new TaggedType(t, tags.toMap)
  }

  val anyType = TaggedType(AnyType)
  val unitType = TaggedType(UnitType)
  val intType = TaggedType(IntType)
  val strType = TaggedType(StrType)
  val boolType = TaggedType(BoolType)

  def basicFuncType(args: List[Arg], outType: TaggedType, isLambda: Boolean) =
    TaggedType(BasicFuncType(args, outType, isLambda))

  def typeLevelFunc(args: List[Arg], f: List[TaggedType] => TaggedType) =
    TaggedType(TypelevelFunc(args, f))
}

sealed trait Type

object Type {
  object IntType extends Type
  object StrType extends Type
  object BoolType extends Type
  object UnitType extends Type
  object AnyType extends Type

  case class TupleType(arr: List[TaggedType]) extends Type

  case class Arg(name: String, tpe: TaggedType)
  sealed trait FuncType extends Type {
    def args: List[Arg]
  }
  case class BasicFuncType(args: List[Arg], outType: TaggedType, isLambda: Boolean) extends FuncType
  case class TypelevelFunc(args: List[Arg], f: List[TaggedType] => TaggedType) extends FuncType

}
