package com.mistlang.lang

object Interpreter {

  import IR._

  val unitVal = "Unit"

  private def evalExpr(env: Env[Any], expr: Expr): Any = {
    expr match {
      case Ident(name) => env.get(name).getOrElse(throw new RuntimeException(s"${name} not found"))
      case Set(name, newValue) =>
        env.set(name, evalExpr(env, newValue))
        ()
      case l: Literal => l.value
      case Lambda(argNames, body) =>
        (args: List[Any]) => {
          val newEnv = argNames.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
            curEnv.put(name, value)
          }
          runAll(newEnv, body)._2
        }
      case Call(func, args) =>
        val f = evalExpr(env, func)
        f match {
          case f: Function1[List[Any], Any] =>
            val resolvedArgs = args.map(evalExpr(env, _))
            f(resolvedArgs)
        }
    }
  }

  def run(env: Env[Any], stmt: IR): (Env[Any], Any) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case Let(name, expr, mutable) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated, mutable), ())
    }
  }

  def runAll(env: Env[Any], stmts: List[IR]): (Env[Any], Any) = {
    stmts.foldLeft((env, (): Any)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt)
    }
  }

}
