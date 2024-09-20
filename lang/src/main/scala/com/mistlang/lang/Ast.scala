package com.mistlang.lang

object Ast {
  case class Program(stmts: List[TopLevelStmt])

  case class FlatProgram(stmts: List[Val])

  sealed trait TopLevelStmt {
    def name: String
  }
  case class ArgDecl(name: String, tpe: Expr)
  case class Namespace(name: String, children: List[TopLevelStmt]) extends TopLevelStmt

  sealed trait FnBodyStmt
  case class Val(id: Int, name: String, expr: Expr) extends FnBodyStmt with TopLevelStmt

  sealed trait Expr extends FnBodyStmt {
    def id: Int
  }
  sealed trait TypeExpr extends Expr
  case class Literal(id: Int, value: Any) extends TypeExpr
  case class Ident(id: Int, name: String) extends TypeExpr
  case class Call(id: Int, func: Expr, args: List[Expr], isInfixCall: Boolean = false) extends TypeExpr
  case class MemberRef(id: Int, expr: Expr, memberName: String) extends TypeExpr
  case class If(id: Int, expr: Expr, success: Expr, fail: Expr) extends Expr
  case class Block(id: Int, stmts: List[FnBodyStmt]) extends Expr
  case class Lambda(
      id: Int,
      args: List[ArgDecl],
      outType: Option[Expr],
      body: Expr,
      isComptime: Boolean
  ) extends Expr
  case class Struct(id: Int, args: List[ArgDecl]) extends TypeExpr

}
