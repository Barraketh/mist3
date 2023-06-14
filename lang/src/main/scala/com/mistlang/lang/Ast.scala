package com.mistlang.lang

object Ast {
  case class Program(topLevelStmts: List[TopLevelStmt], stmts: List[Stmt])

  sealed trait Stmt

  sealed trait TopLevelStmt extends Stmt {
    def name: String
  }
  case class ArgDecl(name: String, tpe: Expr)
  case class Def(lambda: Lambda) extends TopLevelStmt {
    override def name: String = lambda.name.get
  }
  case class Struct(name: String, args: List[ArgDecl]) extends TopLevelStmt
  case class Namespace(name: String, children: List[TopLevelStmt]) extends TopLevelStmt

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
  case class Lambda(id: Int, name: Option[String], args: List[ArgDecl], outType: Option[Expr], body: Expr) extends Expr
}
