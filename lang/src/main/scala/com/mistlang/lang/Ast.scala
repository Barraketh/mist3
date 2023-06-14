package com.mistlang.lang

object Ast {
  case class Program(topLevelStmts: List[TopLevelStmt], stmts: List[Stmt])

  sealed trait TopLevelStmt {
    def name: String
  }
  case class ArgDecl(name: String, tpe: Expr)
  case class Def(name: String, args: List[ArgDecl], outType: Expr, body: Expr) extends TopLevelStmt
  case class Struct(name: String, args: List[ArgDecl]) extends TopLevelStmt
  case class Namespace(name: String, children: List[TopLevelStmt]) extends TopLevelStmt

  sealed trait Stmt
  case class Val(name: String, expr: Expr) extends Stmt

  sealed trait Expr extends Stmt {
    def id: Int
  }
  case class Literal(id: Int, value: Any) extends Expr
  case class Ident(id: Int, name: String) extends Expr
  case class Call(id: Int, func: Expr, args: List[Expr], isInfixCall: Boolean = false) extends Expr
  case class If(id: Int, expr: Expr, success: Expr, fail: Expr) extends Expr
  case class MemberRef(id: Int, expr: Expr, memberName: String) extends Expr
  case class Block(id: Int, stmts: List[Stmt]) extends Expr
}
