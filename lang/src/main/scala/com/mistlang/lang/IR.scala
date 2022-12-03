package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types._

sealed trait IR {
  val tpe: Type
}
object IR {
  sealed trait BodyStmt extends IR
  sealed trait Expr extends BodyStmt
  case class Let(name: String, expr: Expr, isMutable: Boolean) extends BodyStmt {
    override val tpe: Type = UnitTypeInstance
  }
  case class Set(name: String, expr: Expr) extends BodyStmt {
    override val tpe: Type = UnitTypeInstance
  }
  case class Ident(name: String, tpe: Type) extends Expr
  case class Lambda(argNames: List[String], body: List[BodyStmt], tpe: Type) extends Expr
  case class Call(expr: Expr, args: List[Expr], tpe: Type) extends Expr
  case class At(expr: Expr, idx: Int, tpe: Type) extends Expr
  case class If(expr: Expr, success: Expr, fail: Expr, tpe: Type) extends Expr
  case class IntLiteral(i: Int) extends Expr {
    override val tpe: Type = IntLiteralType(i)
  }
  case class StrLiteral(s: String) extends Expr {
    override val tpe: Type = StringLiteralType(s)
  }
  case class BoolLiteral(b: Boolean) extends Expr {
    override val tpe: Type = BoolLiteralType(b)
  }
  case class Null(tpe: Type) extends Expr
}
