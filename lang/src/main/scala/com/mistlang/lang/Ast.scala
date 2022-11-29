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
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr

  case class RecordRow(key: String, value: Expr)
  case class Record(rows: List[RecordRow]) extends Expr

  case class Val(name: String, expr: Expr) extends FnStmt
  case class Def(name: String, args: List[ArgDecl], outType: Expr, body: Expr) extends Ast
  case class Program(defs: List[Def], stmts: List[FnStmt]) extends Ast
}
