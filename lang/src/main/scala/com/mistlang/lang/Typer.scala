package com.mistlang.lang

import com.mistlang.lang.Ast.{FnStmt, Program}
import com.mistlang.lang.RuntimeValue.Type
import Type.{BasicFuncType, UnitType}

object Typer {
  def error(s: String) = throw TypeError(s)

  def checkType(expected: Type, actual: Type, name: String): Unit = {
    if (expected == Type.AnyType || expected == actual) ()
    else error(s"Unexpected type for $name - expected $expected, actual $actual")
  }
  private def compileLambda(tpe: BasicFuncType, body: Ast.Expr, env: Env[RuntimeValue]) = {

    val newEnv = tpe.expected.foldLeft(env.newScope) { case (curEnv, nextArg) =>
      curEnv.put(nextArg._1, nextArg._2)
    }

    val irBody = body match {
      case b: Ast.Block => compileAll(b.stmts, newEnv)._1
      case _            => compileExpr(body, newEnv) :: Nil
    }
    val computedOut = getOutType(irBody)
    checkType(tpe.out, computedOut, "out")

    IR.Lambda(irBody, tpe)
  }

  private def getLambdaType(d: Ast.Def, env: Env[RuntimeValue]): BasicFuncType = {
    val argTypes = d.args.map(arg =>
      Interpreter.evalExpr(env, compileExpr(arg.tpe, env)) match {
        case tpe: Type => arg.name -> tpe
      }
    )
    val outType = Interpreter.evalExpr(env, compileExpr(d.outType, env)) match {
      case tpe: Type => tpe
    }
    BasicFuncType(argTypes, outType)
  }

  private def getOutType(stmts: List[IR]): Type = {
    if (stmts.isEmpty) UnitType
    else stmts.last.tpe
  }

  private def blockLambda(stmts: List[Ast.FnStmt], env: Env[RuntimeValue]): IR.Lambda = {
    val compiledStmts = compileAll(stmts, env)._1
    val outType = getOutType(compiledStmts)
    IR.Lambda(compiledStmts, BasicFuncType(Nil, outType))
  }

  private def compileExpr(expr: Ast.Expr, env: Env[RuntimeValue]): IR.Expr = expr match {
    case Ast.Literal(value) =>
      value match {
        case i: Int     => IR.IntLiteral(i)
        case b: Boolean => IR.BoolLiteral(b)
        case s: String  => IR.StrLiteral(s)
      }

    case Ast.Ident(name) =>
      val value = env.get(name).getOrElse(error(s"$name not found"))
      val tpe = value match {
        case t: RuntimeValue.Type => t
        case other                => error(s"$other is not a type")
      }
      IR.Ident(name, tpe)
    case Ast.Block(stmts) =>
      val lambda = blockLambda(stmts, env)
      IR.Call(lambda, Nil)
    case Ast.If(expr, success, fail) =>
      val successLambda = blockLambda(success :: Nil, env)
      val failLambda = blockLambda(fail :: Nil, env)
      IR.Call(
        compileExpr(Ast.Ident("if"), env),
        List(
          compileExpr(expr, env),
          successLambda,
          failLambda
        )
      )
    case Ast.Call(func, args, _) =>
      val compiledFunc = compileExpr(func, env)
      IR.Call(compiledFunc, args.map(a => compileExpr(a, env)))
  }
  private def compileStmt(stmt: Ast.FnStmt, env: Env[RuntimeValue]): (IR, Env[RuntimeValue]) = stmt match {
    case expr: Ast.Expr => (compileExpr(expr, env), env)
    case Ast.Val(name, expr) =>
      val compiledExpr = compileExpr(expr, env)
      (IR.Let(name, compiledExpr), env.put(name, compiledExpr.tpe))
  }

  private def compileAll(stmts: List[FnStmt], env: Env[RuntimeValue]): (List[IR], Env[RuntimeValue]) = {
    stmts.foldLeft((Nil: List[IR], env)) { case ((curStmts, curEnv), nextStmt) =>
      val compiled = compileStmt(nextStmt, curEnv)
      (curStmts :+ compiled._1, compiled._2)
    }
  }

  def compile(program: Program, env: Env[RuntimeValue]): List[IR] = {
    val tpes = program.defs.map { d =>
      d.name -> getLambdaType(d, env)
    }.toMap

    val forwardRefs =
      program.defs.map(d => IR.Let(d.name, IR.Ident(Interpreter.nullVal, tpes(d.name)), mutable = true))

    val newEnv = program.defs.foldLeft(env) { case (curEnv, nextDef) =>
      curEnv.put(nextDef.name, tpes(nextDef.name))
    }

    val values = program.defs.map { d =>
      IR.Set(d.name, compileLambda(tpes(d.name), d.body, newEnv))
    }
    forwardRefs ::: values ::: compileAll(program.stmts, newEnv)._1
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
