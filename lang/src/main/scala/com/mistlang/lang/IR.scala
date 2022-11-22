package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._
import Type._

sealed trait IR {
  val tpe: Type
}
object IR {
  sealed trait BodyStmt extends IR
  sealed trait Expr extends BodyStmt
  case class Let(name: String, expr: Expr) extends BodyStmt {
    override val tpe: Type = UnitType
  }
  case class Def(name: String, l: Lambda) extends IR {
    override val tpe: Type = l.tpe
  }

  case class Ident(name: String, tpe: Type) extends Expr
  case class Lambda(body: List[BodyStmt], tpe: BasicFuncType) extends Expr
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
