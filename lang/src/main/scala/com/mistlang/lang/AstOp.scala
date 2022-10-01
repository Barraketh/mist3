package com.mistlang.lang

sealed trait AstOp

object AstOp {
  type Ast = Tree[AstOp]

  sealed trait Expr extends AstOp

  case class Literal(value: Any) extends Expr

  case class Ident(name: String) extends Expr
  case class Lambda(argNames: List[String]) extends Expr

  case object Tuple extends Expr
  case object Block extends Expr
  case object Call extends Expr

  case class Val(name: String) extends AstOp
  case class Def(name: String) extends AstOp
}
