package com.mistlang.lang

sealed trait Ast
object Ast {
  case class Program(defs: List[Def], stmts: List[Stmt])
  case class ArgDecl(name: String, tpe: Expr)
  case class Def(name: String, args: List[ArgDecl], outType: Expr, body: List[Stmt]) extends Ast

  sealed trait Stmt extends Ast
  case class Val(name: String, expr: Expr) extends Stmt

  sealed trait Expr extends Stmt
  case class Literal(value: Any) extends Expr
  case class Ident(name: String) extends Expr
  case class Call(func: Expr, args: List[Expr], isInfixCall: Boolean = false) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr
}
