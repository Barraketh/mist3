package com.mistlang.lang

import com.mistlang.lang.Type.FuncType

sealed trait TypedAst {
  def tpe: Type
}

object TypedAst {
  sealed trait Expr extends TypedAst

  case class Literal(value: Any, tpe: Type) extends Expr
  case class Ident(name: String, tpe: Type) extends Expr
  case class Tuple(exprs: List[Expr], tpe: Type) extends Expr
  case class Block(stmts: List[TypedAst], tpe: Type) extends Expr
  case class Call(func: Expr, args: List[Expr], tpe: Type) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr, tpe: Type) extends Expr
  case class Lambda(body: Expr, tpe: FuncType) extends Expr
  case class Synthetic(tpe: Type) extends Expr

  case class Val(name: String, expr: Expr, tpe: Type) extends TypedAst

  case class Def(name: String, func: Lambda, tpe: Type) extends TypedAst

}
