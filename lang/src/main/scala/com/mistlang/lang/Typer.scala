package com.mistlang.lang

import com.mistlang.lang.Ast.{FnStmt, Program}
import com.mistlang.lang.IR.BodyStmt
import com.mistlang.lang.RuntimeValue.Types._
import com.mistlang.lang.RuntimeValue.{BoolVal, RuntimeType, Type}

object TypeCheck {
  import Typer.error
  def checkType(expected: RuntimeType, actual: RuntimeType): Boolean = {
    expected == AnyTypeR || expected == actual
  }

  def checkType(expected: RuntimeValue, actual: RuntimeValue): Boolean = {
    (expected, actual) match {
      case (et: Type, at: Type) => checkType(et.tpe, at.tpe)
      case _                    => error(s"Both $expected and $actual must be types")
    }
  }

  def validateType(expected: RuntimeValue, actual: RuntimeValue, name: String): Unit = {
    if (checkType(expected, actual)) ()
    else error(s"Failed typecheck for $name. Expected: $expected, actual: $actual")
  }

  def intersect(left: Type, right: Type): Type = {
    if (left.tpe != right.tpe) AnyType
    else Type(left.tpe, left.data.toSet.intersect(right.data.toSet).toMap)
  }

  def isMutable(tpe: Type): Boolean = tpe.data.contains("isMutable")
}

object Typer {
  import TypeCheck._
  def error(s: String) = throw TypeError(s)

  def assert(cond: Boolean, message: String): Unit = {
    if (!cond) error(message)
  }

  private def compileLambda(d: Ast.Def, fullType: Type, env: Env[RuntimeValue]) = {
    val tpe = fullType.tpe match {
      case b: BasicFuncTypeR => b
      case _                 => error(s"${fullType} is not a lambda")
    }
    val argNames = d.args.map(_.name)
    val newEnv = argNames.zip(tpe.args).foldLeft(env.newScope) { case (curEnv, nextArg) =>
      curEnv.put(nextArg._1, nextArg._2)
    }

    val irBody = d.body match {
      case b: Ast.Block => compileAll(b.stmts, newEnv)._1
      case _            => compileExpr(d.body, newEnv) :: Nil
    }
    val computedOut = getOutType(irBody)
    validateType(tpe.out, computedOut, "out")

    IR.Lambda(argNames, irBody, Type(tpe))
  }

  private def getLambdaType(d: Ast.Def, env: Env[RuntimeValue]): Type = {
    val argTypes = d.args.map(arg => Interpreter.evalExpr(env, arg.tpe).getType)
    val outType = Interpreter.evalExpr(env, d.outType) match {
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
    IR.Lambda(Nil, compiledStmts, BasicFuncType(Nil, outType))
  }

  private def compileCall(c: Ast.Call, env: Env[RuntimeValue]): IR.Expr = {
    val compiledFunc = compileExpr(c.func, env)
    val funcTpe = compiledFunc.tpe.tpe match {
      case f: BasicFuncTypeR => f
      case _                 => error(s"Cannot call an object of type ${compiledFunc.tpe}")
    }
    val compiledArgs = c.args.map(a => compileExpr(a, env))
    funcTpe.args.zip(compiledArgs.map(_.tpe)).zipWithIndex.foreach { case ((expected, actual), idx) =>
      validateType(expected, actual, s"arg $idx")
    }
    IR.Call(compiledFunc, compiledArgs, funcTpe.out)
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
      IR.Ident(name, value.getType)
    case Ast.Block(stmts) =>
      val lambda = blockLambda(stmts, env)
      IR.Call(lambda, Nil, getOutType(lambda.body))
    case Ast.If(expr, success, fail) =>
      val resolvedExpr = compileExpr(expr, env)
      val compiledSuccess = compileExpr(success, env)
      val compiledFail = compileExpr(fail, env)
      IR.If(resolvedExpr, compiledSuccess, compiledFail, intersect(compiledSuccess.tpe, compiledFail.tpe))
    case c: Ast.Call => compileCall(c, env)
  }
  private def compileStmt(stmt: Ast.FnStmt, env: Env[RuntimeValue]): (BodyStmt, Env[RuntimeValue]) = stmt match {
    case expr: Ast.Expr => (compileExpr(expr, env), env)
    case Ast.Val(name, expr) =>
      val compiledExpr = compileExpr(expr, env)
      (IR.Let(name, compiledExpr, isMutable = false), env.put(name, compiledExpr.tpe))
  }

  private def compileAll(stmts: List[FnStmt], env: Env[RuntimeValue]): (List[BodyStmt], Env[RuntimeValue]) = {
    stmts.foldLeft((Nil: List[BodyStmt], env)) { case ((curStmts, curEnv), nextStmt) =>
      val compiled = compileStmt(nextStmt, curEnv)
      (curStmts :+ compiled._1, compiled._2)
    }
  }

  def compile(program: Program, env: Env[RuntimeValue]): List[IR] = {
    val tpes = program.defs.map { d =>
      d.name -> (getLambdaType(d, env) + ("isMutable" -> BoolVal(true)))
    }.toMap

    val newEnv = program.defs.foldLeft(env) { case (curEnv, nextDef) =>
      curEnv.put(nextDef.name, tpes(nextDef.name))
    }

    val defs = program.defs.map { d =>
      IR.Let(d.name, IR.Null(tpes(d.name)), isMutable = true)
    }

    val values = program.defs.map { d =>
      IR.Set(d.name, compileLambda(d, tpes(d.name), newEnv))
    }
    defs ::: values ::: compileAll(program.stmts, newEnv)._1
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
