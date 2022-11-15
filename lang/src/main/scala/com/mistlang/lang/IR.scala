package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

sealed trait IR {
  val tpe: Type
}
object IR {
  sealed trait Expr extends IR
  case class Let(name: String, expr: Expr, mutable: Boolean = false) extends IR {
    override val tpe: Type = UnitType
  }
  case class Set(name: String, expr: Expr) extends Expr {
    override val tpe: Type = UnitType
  }
  case class Ident(name: String, tpe: Type) extends Expr
  case class Lambda(body: List[IR], tpe: BasicFuncType) extends Expr
  case class Call(expr: Expr, args: List[Expr]) extends Expr {
    override val tpe: Type = {
      expr.tpe match {
        case f: FuncType => f.f(args.map(_.tpe))
        case _           => Typer.error(s"Cannot call $expr")
      }
    }
  }
  case class IntLiteral(i: Int) extends Expr {
    override val tpe: Type = IntType
  }
  case class StrLiteral(s: String) extends Expr {
    override val tpe: Type = StrType
  }
  case class BoolLiteral(b: Boolean) extends Expr {
    override val tpe: Type = BoolType
  }
}
