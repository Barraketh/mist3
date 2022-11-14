package com.mistlang.lang

import com.mistlang.lang.Ast.Program

object Typer {
  def compileLambda(args: List[Ast.ArgDecl], body: Ast.Expr) = {
    val irBody = body match {
      case b: Ast.Block => b.stmts.map(compileFnStmt)
      case _            => compileExpr(body) :: Nil
    }
    IR.Lambda(args.map(_.name), irBody)
  }

  def compileExpr(expr: Ast.Expr): IR.Expr = expr match {
    case Ast.Literal(value) => IR.Literal(value)
    case Ast.Ident(name)    => IR.Ident(name)
    case Ast.Block(stmts) =>
      IR.Call(
        IR.Lambda(Nil, stmts.map(compileFnStmt)),
        Nil
      )
    case Ast.If(expr, success, fail) =>
      IR.Call(
        IR.Ident("if"),
        List(
          compileExpr(expr),
          IR.Lambda(Nil, compileExpr(success) :: Nil),
          IR.Lambda(Nil, compileExpr(fail) :: Nil)
        )
      )
    case Ast.Call(func, args, _) => IR.Call(compileExpr(func), args.map(compileExpr))
  }
  def compileFnStmt(stmt: Ast.FnStmt): IR = stmt match {
    case expr: Ast.Expr      => compileExpr(expr)
    case Ast.Val(name, expr) => IR.Let(name, compileExpr(expr))
  }
  def compile(program: Program): List[IR] = {
    val forwardRefs = program.defs.map(d => IR.Let(d.name, IR.Ident(Interpreter.unitVal), mutable = true))
    val values = program.defs.map { d =>
      IR.Set(d.name, compileLambda(d.args, d.body))
    }
    forwardRefs ::: values ::: program.stmts.map(compileFnStmt)
  }
}
