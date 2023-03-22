package com.mistlang.lang

object Ast {
  case class Program(topLevelStmts: List[TopLevelStmt], stmts: List[Stmt])

  sealed trait TopLevelStmt {
    def name: String
  }
  case class ArgDecl(name: String, tpe: Expr)
  case class Def(name: String, args: List[ArgDecl], outType: Expr, body: Expr) extends TopLevelStmt
  case class Struct(name: String, args: List[ArgDecl]) extends TopLevelStmt

  sealed trait Stmt
  case class Val(name: String, expr: Expr) extends Stmt

  sealed trait Expr extends Stmt
  case class Literal(value: Any) extends Expr
  case class Ident(name: String) extends Expr
  case class Call(func: Expr, args: List[Expr], isInfixCall: Boolean = false) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr
  case class MemberRef(expr: Expr, memberName: String) extends Expr
  case class Block(stmts: List[Stmt]) extends Expr
}
