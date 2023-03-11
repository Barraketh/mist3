package com.mistlang.java.codegen

object JavaAst {
  case class Program(functions: List[Def])
  case class Arg(name: String, tpe: String)
  case class Def(funcType: String, name: String, args: List[Arg], outType: String, body: List[Stmt])

  sealed trait Stmt
  case class Let(name: String, tpe: String, expr: Expr) extends Stmt
  case class Return(expr: Expr) extends Stmt

  sealed trait Expr extends Stmt
  case class Call(expr: Expr, args: List[Expr]) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr
  case class Ident(name: String) extends Expr
  case class IntLiteral(i: Int) extends Expr
  case class BoolLiteral(b: Boolean) extends Expr
  case class StrLiteral(s: String) extends Expr
}
