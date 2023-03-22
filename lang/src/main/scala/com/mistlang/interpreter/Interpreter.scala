package com.mistlang.interpreter

import RuntimeValue._

object InterpreterAst {
  sealed trait Ast[+A]
  sealed trait Stmt[+A] extends Ast[A]
  case class Let[A](name: String, expr: Expr[A]) extends Stmt[A]
  case class Set[A](name: String, expr: Expr[A]) extends Stmt[A]
  sealed trait Expr[+A] extends Stmt[A]
  case class Literal[A](value: A) extends Expr[A]
  case class Ident(name: String) extends Expr[Nothing]
  case class Call[A](func: Expr[A], args: List[Expr[A]]) extends Expr[A]
  case class Lambda[A](args: List[String], body: Expr[A]) extends Expr[A]
  case class Block[A](stmts: List[Stmt[A]]) extends Expr[A]

}
class Interpreter[A] {

  import InterpreterAst._

  def evalExpr(env: Env[RuntimeValue[A]], expr: Expr[A]): RuntimeValue[A] = {
    expr match {
      case i: Ident       => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case Literal(value) => Strict(value)
      case b: Block[A]    => runAll(env.newScope, b.stmts)
      case c: Call[A] =>
        val f = evalExpr(env, c.func)
        f match {
          case f: Func[A] =>
            val resolvedArgs = c.args.map(evalExpr(env, _))
            f.f(resolvedArgs)
        }
      case l: Lambda[A] => buildFunc(l, env)
    }
  }

  private def buildFunc(d: Lambda[A], env: Env[RuntimeValue[A]]): Func[A] = Func((args: List[RuntimeValue[A]]) => {
    val newEnv = {
      d.args.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, value)
      }
    }
    evalExpr(newEnv.newScope, d.body)
  })

  def run(env: Env[RuntimeValue[A]], stmt: Ast[A]): (Env[RuntimeValue[A]], RuntimeValue[A]) = {
    stmt match {
      case e: Expr[A] => (env, evalExpr(env, e))
      case l: Let[A] =>
        val evaluated = evalExpr(env, l.expr)
        (env.put(l.name, evaluated), UnitVal)
      case s: Set[A] =>
        val evaluated = evalExpr(env, s.expr)
        env.set(s.name, evaluated)
        (env, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue[A]], stmts: List[Ast[A]]): RuntimeValue[A] = {
    stmts
      .foldLeft((env, UnitVal: RuntimeValue[A])) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt)
      }
      ._2
  }

}
