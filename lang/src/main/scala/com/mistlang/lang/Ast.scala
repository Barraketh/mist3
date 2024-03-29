package com.mistlang.lang

sealed trait Ast
object Ast {
  sealed trait FnStmt extends Ast
  sealed trait Expr extends FnStmt

  case class ArgDecl(name: String, tpe: Expr)

  case class Literal(value: Any) extends Expr
  case class Ident(name: String) extends Expr
  case class Block(stmts: List[FnStmt]) extends Expr
  case class Call(func: Expr, args: List[Expr], isInfixCall: Boolean = false) extends Expr
  case class Lambda(args: List[ArgDecl], outType: Option[Expr], body: Expr, name: Option[String], isInline: Boolean)
    extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr

  case class Val(name: String, expr: Expr) extends FnStmt
  case class Def(name: String, expr: Expr) extends Ast

  case class Program(defs: List[Def], stmts: List[FnStmt]) extends Ast
}
