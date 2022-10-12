package com.mistlang.lang

sealed trait Ast
object Ast {
  sealed trait Expr extends Ast

  case class ArgDecl(name: String, tpe: Expr)

  case class Literal(value: Any) extends Expr
  case class Ident(name: String) extends Expr
  case class Tuple(exprs: List[Expr]) extends Expr
  case class Block(stmts: List[Ast]) extends Expr
  case class Call(func: Expr, args: List[Expr]) extends Expr
  case class Lambda(args: List[ArgDecl], outType: Option[Expr], body: Expr, name: Option[String]) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr

  case class Val(name: String, expr: Expr) extends Ast
  case class Def(name: String, func: Lambda) extends Ast
}
