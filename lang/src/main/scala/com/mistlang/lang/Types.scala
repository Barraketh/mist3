package com.mistlang.lang

import com.mistlang.lang2.Typer.TypeObject

sealed trait Type

object Types {
  case object AnyType extends Type
  case object IntType extends Type
  case object StrType extends Type
  case object BoolType extends Type
  case object UnitType extends Type
  case object NullType extends Type

  case class FuncType(args: List[Type], out: Type) extends Type
  case class TypeConstructor(f: List[TypeObject] => TypeObject) extends Type
  object TypeConstructor {
    def make(f: List[Type] => Type): TypeConstructor = new TypeConstructor((typeObjects) => {
      TypeObject(f(typeObjects.map(_.tpe)))
    })
  }
  case class StructType(name: String, namespace: String, args: List[(String, Type)]) extends Type
  case class NamespaceType(children: Map[String, Type]) extends Type
}
