package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Dict
import com.mistlang.lang.Types._

sealed trait IR {
  val tpe: Dict
}
object IR {
  sealed trait BodyStmt extends IR
  sealed trait Expr extends BodyStmt
  case class Let(name: String, expr: Expr, isMutable: Boolean) extends BodyStmt {
    override val tpe: Dict = UnitType
  }
  case class Set(name: String, expr: Expr) extends BodyStmt {
    override val tpe: Dict = UnitType
  }
  case class Ident(name: String, tpe: Dict) extends Expr
  case class Lambda(body: List[BodyStmt], tpe: Dict) extends Expr
  case class Call(expr: Expr, args: List[Expr]) extends Expr {
    override val tpe: Dict = {
      Typer.validateType(FuncType, expr.tpe, "function")
      expr.tpe("f").getFunc.f(args.map(_.tpe)).getDict
    }
  }
  case class If(expr: Expr, success: Expr, fail: Expr) extends Expr {
    override val tpe: Dict = Types.intersect(success.tpe, fail.tpe)
  }

  case class DictRowIR(key: String, value: Expr)
  case class DictIR(rows: List[DictRowIR]) extends Expr {
    override val tpe: Dict = DictLiteralType(rows.map(r => (r.key, r.value.tpe)): _*)
  }
  case class IntLiteral(i: Int) extends Expr {
    override val tpe: Dict = IntLiteralType(i)
  }
  case class StrLiteral(s: String) extends Expr {
    override val tpe: Dict = StringLiteralType(s)
  }
  case class BoolLiteral(b: Boolean) extends Expr {
    override val tpe: Dict = BoolLiteralType(b)
  }
  case class Null(tpe: Dict) extends Expr
}
