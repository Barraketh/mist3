package com.mistlang.lang

import com.mistlang.lang.Types._

sealed trait IR {
  val tpe: Type
}
object IR {
  case class Program(defs: List[Def], body: List[BodyStmt])
  case class Arg(name: String, tpe: Type)
  case class Def(name: String, args: List[Arg], outType: Type, body: List[BodyStmt]) extends IR {
    override val tpe: FuncType = FuncType(args.map(_.tpe), outType)
  }
  sealed trait BodyStmt extends IR
  case class Let(name: String, expr: Expr) extends BodyStmt {
    override val tpe: Type = UnitType
  }
  sealed trait Expr extends BodyStmt
  case class Ident(name: String, tpe: Type) extends Expr
  case class Call(expr: Expr, args: List[Expr], tpe: Type) extends Expr
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
}
