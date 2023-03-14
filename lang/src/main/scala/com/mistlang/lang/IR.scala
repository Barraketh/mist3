package com.mistlang.lang

import com.mistlang.lang.Types._

object IR {
  case class Program(structs: List[Struct], defs: List[Def], body: List[BodyStmt])
  case class Def(name: String, args: List[String], body: List[BodyStmt], tpe: FuncType)
  case class Struct(tpe: StructType)
  sealed trait BodyStmt
  case class Let(name: String, expr: Expr) extends BodyStmt
  sealed trait Expr extends BodyStmt {
    def tpe: Type
  }
  case class Ident(name: String, tpe: Type) extends Expr
  case class Call(expr: Expr, args: List[Expr], tpe: Type) extends Expr
  case class New(args: List[Expr], tpe: Type) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr {
    override val tpe = if (success.tpe == fail.tpe) success.tpe else AnyType
  }
  case class IntLiteral(i: Int) extends Expr {
    override val tpe: Type = IntType
  }
  case class StrLiteral(s: String) extends Expr {
    override val tpe: Type = StrType
  }
  case class BoolLiteral(b: Boolean) extends Expr {
    override val tpe: Type = BoolType
  }
  case class MemberRef(expr: Expr, memberName: String, tpe: Type) extends Expr
}
