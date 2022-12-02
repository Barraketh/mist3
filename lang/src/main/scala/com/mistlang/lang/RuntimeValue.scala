package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types.BasicFuncType

sealed trait RuntimeValue {
  def getType: Type = {
    this match {
      case t: Type => t
      case _       => Typer.error(s"$this is not a type")
    }
  }
}
object RuntimeValue {
  sealed trait Primitive extends RuntimeValue
  case class StrVal(value: String) extends Primitive
  case class BoolVal(value: Boolean) extends Primitive
  case class IntVal(value: Int) extends Primitive
  case object UnitVal extends Primitive
  case object NullVal extends Primitive
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue
  case class Type(tpe: RuntimeType, data: Map[String, RuntimeValue] = Map.empty) extends RuntimeValue {
    def +(pairs: (String, RuntimeValue)*): Type = copy(data = data ++ pairs)

    def getFuncType: BasicFuncType = tpe match {
      case b: BasicFuncType => b
      case _                => Typer.error(s"$this is not a func")
    }
  }

  sealed trait RuntimeType

  object Types {

    case object AnyType extends RuntimeType
    case object IntType extends RuntimeType
    case object StrType extends RuntimeType
    case object BoolType extends RuntimeType
    case object UnitType extends RuntimeType
    case class BasicFuncType(args: List[Type], out: Type) extends RuntimeType

    val AnyTypeInstance: Type = Type(AnyType)
    val IntTypeInstance: Type = Type(IntType)
    val BoolTypeInstance: Type = Type(BoolType)
    val StrTypeInstance: Type = Type(StrType)
    val UnitTypeInstance: Type = Type(UnitType)

    def IntLiteralType(i: Int): Type = IntTypeInstance + ("value" -> IntVal(i))
    def StringLiteralType(s: String): Type = StrTypeInstance + ("value" -> StrVal(s))
    def BoolLiteralType(b: Boolean): Type = BoolTypeInstance + ("value" -> BoolVal(b))

    def BasicFuncTypeInstance(args: List[Type], out: Type): Type = Type(BasicFuncType(args, out))
    def op(a: RuntimeType, b: RuntimeType, out: RuntimeType): Type =
      BasicFuncTypeInstance(List(Type(a), Type(b)), Type(out))

  }

}
