package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._

trait Visitor[T] {
  def unit: T
  def evalTopLevel: Boolean
  def asT(a: Any): T
  def literal(l: Literal): T
  def call(c: Call, env: Env[RuntimeValue]): T
  def lambda(l: Lambda, env: Env[RuntimeValue]): T
  def cache(id: Int, t: T): Unit
  def `if`(i: If, env: Env[RuntimeValue]): T
  def memberRef(m: MemberRef, env: Env[RuntimeValue]): T
  def struct(s: Struct, env: Env[RuntimeValue]): T
  def namespace(n: Namespace, env: Env[RuntimeValue]): T
  def evalExpr(e: Expr, env: Env[RuntimeValue]): T = Evaluator.evalExpr(env, e, this)

}

object Evaluator {
  def error(s: String) = throw new RuntimeException(s)

  def evalExpr[T](env: Env[RuntimeValue], expr: Expr, v: Visitor[T]): T = {
    val res = expr match {
      case l: Literal => v.literal(l)
      case i: Ident =>
        v.asT(env.get(i.name).getOrElse(error(s"${i.name} not found")).value)
      case c: Call      => v.call(c, env)
      case l: Lambda    => v.lambda(l, env)
      case b: Block     => runAll(env.newScope, b.stmts, v)._2
      case i: If        => v.`if`(i, env)
      case m: MemberRef => v.memberRef(m, env)

    }
    v.cache(expr.id, res)
    res
  }

  def run[T](env: Env[RuntimeValue], stmt: Stmt, v: Visitor[T]): (Env[RuntimeValue], T) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e, v))
      case Val(name, expr) =>
        val evaluated = evalExpr(env, expr, v)
        (env.put(name, Strict(evaluated)), v.unit)
      case d: Def =>
        env.set(d.name, Lazy(() => evalExpr(env, d.lambda, v)))
        (env, v.unit)
      case s: Struct =>
        env.set(s.name, Lazy(() => v.struct(s, env)))
        (env, v.unit)
      case n: Namespace =>
        val namespaceEnv = runAll(env.newScope, n.children, v)._1
        env.set(n.name, Lazy(() => v.namespace(n, namespaceEnv)))
        (env, v.unit)
    }
  }

  def runAll[T](env: Env[RuntimeValue], stmts: List[Stmt], v: Visitor[T]): (Env[RuntimeValue], T) = {
    val topLevel = stmts.collect { case c: TopLevelStmt => c.name }
    val newEnv = topLevel.foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }

    stmts
      .foldLeft((newEnv, v.unit)) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt, v)
      }
  }
}
