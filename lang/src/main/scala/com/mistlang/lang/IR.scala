package com.mistlang.lang

sealed trait IR
object IR {
  sealed trait Expr extends IR
  case class Let(name: String, expr: Expr, mutable: Boolean = false) extends IR
  case class Set(name: String, expr: Expr) extends Expr
  case class Ident(name: String) extends Expr
  case class Lambda(args: List[String], body: List[IR]) extends Expr
  case class Call(func: Expr, args: List[Expr]) extends Expr
  case class Literal(value: Any) extends Expr
}
