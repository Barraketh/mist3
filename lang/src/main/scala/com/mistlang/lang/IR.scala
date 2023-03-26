package com.mistlang.lang

import com.mistlang.lang.Types._

object IR {
  case class Program(stmts: List[TopLevelStmt], body: Expr)
  sealed trait TopLevelStmt
  case class Def(name: String, args: List[String], body: Expr, tpe: FuncType) extends TopLevelStmt
  case class Struct(tpe: StructType) extends TopLevelStmt
  case class Namespace(name: String, stmts: List[TopLevelStmt]) extends TopLevelStmt
  sealed trait Stmt
  case class Let(name: String, expr: Expr) extends Stmt
  sealed trait Expr extends Stmt {
    def tpe: Type
  }
  case class Ident(name: String, tpe: Type) extends Expr
  case class Call(expr: Expr, args: List[Expr], tpe: Type) extends Expr
  case class New(args: List[Expr], tpe: Type) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr {
    override val tpe = if (success.tpe == fail.tpe) success.tpe else AnyType
  }
  case class Block(stmts: List[Stmt]) extends Expr {
    override val tpe: Type = stmts.lastOption match {
      case Some(e: Expr) => e.tpe
      case _             => UnitType
    }
  }
  sealed trait Literal extends Expr
  case class IntLiteral(i: Int) extends Literal {
    override val tpe: Type = IntType
  }
  case class StrLiteral(s: String) extends Literal {
    override val tpe: Type = StrType
  }
  case class BoolLiteral(b: Boolean) extends Literal {
    override val tpe: Type = BoolType
  }
  case class MemberRef(expr: Expr, memberName: String, tpe: Type) extends Expr
}
