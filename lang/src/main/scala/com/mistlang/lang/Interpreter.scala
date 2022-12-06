package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import Ast._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case Ast.Literal(value) =>
        value match {
          case i: Int     => IntVal(i)
          case s: String  => StrVal(s)
          case b: Boolean => BoolVal(b)
        }

      case c: Call =>
        val f = evalExpr(env, c.func)
        f match {
          case f: Func =>
            val resolvedArgs = c.args.map(evalExpr(env, _))
            f.f(resolvedArgs)
        }
      case i: If =>
        val cond = evalExpr(env, i.expr)
        cond match {
          case BoolVal(true)  => evalExpr(env, i.success)
          case BoolVal(false) => evalExpr(env, i.fail)
        }
      case b: Block => runAll(env.newScope, b.stmts)
    }
  }

  private def buildFunc(d: Def, env: Env[RuntimeValue]): Func = Func((args: List[RuntimeValue]) => {
    val newEnv =
      d.args.map(_.name).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, value)
      }
    evalExpr(newEnv, d.body)
  })

  def run(env: Env[RuntimeValue], stmt: Ast): (Env[RuntimeValue], RuntimeValue) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case Val(name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated), UnitVal)
      case d: Def =>
        val func = buildFunc(d, env)
        env.set(d.name, func)
        (env, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[Ast]): RuntimeValue = {
    stmts
      .foldLeft((env, UnitVal: RuntimeValue)) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt)
      }
      ._2
  }

  def runAll(env: Env[RuntimeValue], program: Program): RuntimeValue = {
    val newEnv = program.defs.foldLeft(env) { case (curEnv, d) => curEnv.put(d.name, NullVal) }
    runAll(newEnv, program.defs ::: program.stmts)
  }

}
