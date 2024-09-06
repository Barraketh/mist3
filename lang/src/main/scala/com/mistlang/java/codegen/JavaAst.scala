package com.mistlang.java.codegen

object JavaAst {
  case class Program(stmts: List[TopLevelStmt])
  sealed trait TopLevelStmt
  case class Arg(name: String, tpe: String)
  case class Def(name: String, lambda: Lambda) extends TopLevelStmt
  case class Struct(name: String, args: List[Arg]) extends TopLevelStmt
  sealed trait Stmt
  case class Let(name: String, tpe: String, expr: Expr) extends Stmt
  case class Set(name: String, expr: Expr) extends Stmt
  case class Return(expr: Expr) extends Stmt
  case class IfStmt(cond: Expr, success: List[Stmt], fail: List[Stmt]) extends Stmt
  case class Decl(name: String, tpe: String) extends Stmt
  case class Block(stmts: List[Stmt]) extends Stmt

  sealed trait Expr extends Stmt
  case class Call(expr: Expr, args: List[Expr]) extends Expr
  case class New(tpe: String, args: List[Expr]) extends Expr
  case class IfExpr(expr: Expr, success: Expr, fail: Expr) extends Expr
  case class Ident(name: String) extends Expr
  case class IntLiteral(i: Int) extends Expr
  case class BoolLiteral(b: Boolean) extends Expr
  case class StrLiteral(s: String) extends Expr
  case class MemberRef(expr: Expr, memberName: String) extends Expr
  case class Lambda(funcType: String, args: List[Arg], outType: String, body: List[Stmt]) extends Expr
}
