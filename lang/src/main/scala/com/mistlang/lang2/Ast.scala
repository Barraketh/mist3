package com.mistlang.lang2

object Ast {
  sealed trait Stmt
  sealed trait EnvOp extends Stmt {
    def name: String
    def expr: Expr
    def isLazy: Boolean
  }
  case class Let(name: String, expr: Expr, isLazy: Boolean) extends EnvOp
  case class Set(name: String, expr: Expr, isLazy: Boolean) extends EnvOp
  sealed trait Expr extends Stmt {
    def id: Int
  }
  case class Literal(id: Int, value: Any) extends Expr
  case class Ident(id: Int, name: String) extends Expr
  case class Call(id: Int, func: Expr, args: List[Expr]) extends Expr
  case class ArgDecl(name: String, tpe: Expr)
  case class Lambda(id: Int, args: List[ArgDecl], outType: Option[Expr], body: Expr, name: Option[String]) extends Expr
  case class Block(id: Int, stmts: List[Stmt]) extends Expr
  case class As(id: Int, expr: Expr, tpeExpr: Expr) extends Expr

}
