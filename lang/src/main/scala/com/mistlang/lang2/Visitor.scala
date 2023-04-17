package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang2.Ast._

trait Visitor[T] {
  def unit: T
  def asT(a: Any): T
  def literal(l: Literal): T
  def call(c: Call, env: Env[RuntimeValue]): T
  def lambda(l: Lambda, env: Env[RuntimeValue]): T
  def as(a: As, env: Env[RuntimeValue]): T
  def cache(id: Int, t: T): Unit
  def evalExpr(e: Expr, env: Env[RuntimeValue]): T = Evaluator.evalExpr(env, e, this)

}

object Evaluator {
  def error(s: String) = throw new RuntimeException(s)

  def evalExpr[T](env: Env[RuntimeValue], expr: Expr, v: Visitor[T]): T = {
    val res = expr match {
      case l: Literal => v.literal(l)
      case i: Ident =>
        v.asT(env.get(i.name).getOrElse(error(s"${i.name} not found")).value)
      case c: Call   => v.call(c, env)
      case l: Lambda => v.lambda(l, env)
      case a: As     => v.as(a, env)
      case b: Block  => runAll(env.newScope, b.stmts, v)._2
    }
    v.cache(expr.id, res)
    res
  }

  def run[T](env: Env[RuntimeValue], stmt: Stmt, v: Visitor[T]): (Env[RuntimeValue], T) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e, v))
      case e: EnvOp =>
        val f: (String, RuntimeValue) => Env[RuntimeValue] = e match {
          case _: Let => env.put
          case _: Set =>
            (name, value) =>
              env.set(name, value)
              env
        }
        val nextEnv = if (!e.isLazy) {
          val evaluated = evalExpr(env, e.expr, v)
          f(e.name, Strict(evaluated))
        } else {
          f(e.name, Lazy(() => evalExpr(env, e.expr, v)))
        }
        (nextEnv, v.unit)
    }
  }

  def runAll[T](env: Env[RuntimeValue], stmts: List[Stmt], v: Visitor[T]): (Env[RuntimeValue], T) = {
    stmts
      .foldLeft((env, v.unit)) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt, v)
      }
  }
}
