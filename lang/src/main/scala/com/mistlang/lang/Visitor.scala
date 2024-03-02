package com.mistlang.lang

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
  def struct(s: Struct, path: String, env: Env[RuntimeValue]): T
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
    }
  }

  def runAll[T](env: Env[RuntimeValue], stmts: List[Stmt], v: Visitor[T]): (Env[RuntimeValue], T) = {
    stmts.foldLeft((env, v.unit)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt, v)
    }
  }

  def runTopLevel[T](env: Env[RuntimeValue], stmt: TopLevelStmt, path: String, v: Visitor[T]): Unit = {
    stmt match {
      case d: Def    => env.set(d.name, Lazy(() => evalExpr(env, d.lambda, v)))
      case s: Struct => env.set(s.name, Lazy(() => v.struct(s, path, env)))
      case n: Namespace =>
        val newPath = if (path.isEmpty) n.name else path + "." + n.name
        val namespaceEnv = runAllTopLevel(env.newScope, n.children, newPath, v)
        env.set(n.name, Lazy(() => v.namespace(n, namespaceEnv)))
    }
  }

  def runAllTopLevel[T](
      env: Env[RuntimeValue],
      stmts: List[TopLevelStmt],
      path: String,
      v: Visitor[T]
  ): Env[RuntimeValue] = {
    val newEnv = stmts.map(_.name).foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }
    stmts.foreach(stmt => runTopLevel(newEnv, stmt, path, v))
    if (v.evalTopLevel) {
      stmts.foreach(s => newEnv.get(s.name).foreach(_.value))
    }
    newEnv
  }

  def runProgram[T](env: Env[RuntimeValue], p: Program, v: Visitor[T]): T = {
    val nextEnv = runAllTopLevel(env, p.topLevelStmts, "", v)
    runAll(nextEnv, p.stmts, v)._2
  }
}
