package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types.{AnyType, FuncType, UnitType}

object TypeCheck {

  def checkType(expected: Type, actual: Type): Boolean = {
    expected == AnyType || expected == actual
  }
  def validateType(expected: Type, actual: Type, name: String): Unit = {
    if (checkType(expected, actual)) ()
    else Typer.error(s"Failed typecheck for $name. Expected: $expected, actual: $actual")
  }
}
object Typer {
  type TypeEnv = Env[RuntimeValue]

  def error(s: String) = throw TypeError(s)

  private def compileDef(d: Ast.Def, funcType: FuncType, env: TypeEnv): IR.Def = {

    val argNames = d.args.map(_.name)
    val newEnv = argNames.zip(funcType.args).foldLeft(env.newScope) { case (curEnv, nextArg) =>
      curEnv.put(nextArg._1, nextArg._2)
    }
    val irBody = compileAll(d.body, newEnv)._1
    val outType = irBody.lastOption.map(_.tpe).getOrElse(UnitType)
    TypeCheck.validateType(funcType.out, outType, "out")

    IR.Def(
      d.name,
      d.args.zip(funcType.args).map { case (arg, tpe) => IR.Arg(arg.name, tpe) },
      funcType.out,
      irBody
    )
  }

  private def getFuncType(d: Ast.Def, env: TypeEnv): FuncType = {
    val argTypes = d.args.map { arg =>
      Interpreter.evalExpr(env, arg.tpe) match { case t: Type => t }
    }
    val outType = Interpreter.evalExpr(env, d.outType) match {
      case tpe: Type => tpe
    }
    FuncType(argTypes, outType)
  }

  private def compileCall(c: Ast.Call, env: TypeEnv): IR.Expr = {
    val compiledFunc = compileExpr(c.func, env)

    compiledFunc.tpe match {
      case f: FuncType =>
        val compiledArgs = c.args.map(a => compileExpr(a, env))
        f.args.zip(compiledArgs).foreach { case (expectedType, arg) =>
          TypeCheck.validateType(expectedType, arg.tpe, "")
        }
        IR.Call(compiledFunc, compiledArgs, f.out)
      case _ => error(s"Cannot call an object of type ${compiledFunc.tpe}")
    }

  }

  private def compileExpr(expr: Ast.Expr, env: TypeEnv): IR.Expr = expr match {
    case Ast.Literal(value) =>
      value match {
        case i: Int     => IR.IntLiteral(i)
        case b: Boolean => IR.BoolLiteral(b)
        case s: String  => IR.StrLiteral(s)
      }

    case Ast.Ident(name) =>
      val value = env.get(name).getOrElse(error(s"$name not found")) match {
        case t: Type => t
      }
      IR.Ident(name, value)
    case Ast.If(expr, success, fail) =>
      val resolvedExpr = compileExpr(expr, env)
      val compiledSuccess = compileExpr(success, env)
      val compiledFail = compileExpr(fail, env)
      IR.If(resolvedExpr, compiledSuccess, compiledFail)
    case c: Ast.Call => compileCall(c, env)
  }

  private def compileStmt(stmt: Ast.BodyStmt, env: TypeEnv): (IR.BodyStmt, TypeEnv) = stmt match {
    case expr: Ast.Expr => (compileExpr(expr, env), env)
    case Ast.Val(name, expr) =>
      val compiledExpr = compileExpr(expr, env)
      (IR.Let(name, compiledExpr), env.put(name, compiledExpr.tpe))
  }

  private def compileAll(stmts: List[Ast.BodyStmt], env: TypeEnv): (List[IR.BodyStmt], TypeEnv) = {
    stmts.foldLeft((Nil: List[IR.BodyStmt], env)) { case ((curStmts, curEnv), nextStmt) =>
      val compiled = compileStmt(nextStmt, curEnv)
      (curStmts :+ compiled._1, compiled._2)
    }
  }

  def compile(program: Ast.Program, env: TypeEnv): IR.Program = {
    val tpes = program.defs.map { d =>
      d.name -> getFuncType(d, env)
    }.toMap

    val newEnv = program.defs.foldLeft(env) { case (curEnv, nextDef) =>
      curEnv.put(nextDef.name, tpes(nextDef.name))
    }

    val defs = program.defs.map(d => compileDef(d, tpes(d.name), newEnv))

    IR.Program(defs, compileAll(program.stmts, newEnv)._1)
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
