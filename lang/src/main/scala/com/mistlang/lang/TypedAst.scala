package com.mistlang.lang

sealed trait TypedAst {
  def tpe: TaggedType
}

object TypedAst {
  sealed trait Expr extends TypedAst

  case class Literal(value: Any, tpe: TaggedType) extends Expr
  case class Ident(name: String, tpe: TaggedType) extends Expr
  case class Block(stmts: List[TypedAst], tpe: TaggedType) extends Expr
  case class Call(func: Expr, args: List[Expr], tpe: TaggedType) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr, tpe: TaggedType) extends Expr
  case class Lambda(body: Expr, tpe: TaggedType) extends Expr
  case class Synthetic(tpe: TaggedType) extends Expr

  case class Val(name: String, expr: Expr, tpe: TaggedType) extends TypedAst

  case class Def(name: String, func: Lambda, tpe: TaggedType) extends TypedAst

}
