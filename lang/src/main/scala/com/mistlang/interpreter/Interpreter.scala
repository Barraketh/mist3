package com.mistlang.interpreter

import com.mistlang.interpreter.RuntimeValue._

object InterpreterAst {
  sealed trait Ast
  sealed trait Stmt extends Ast
  sealed trait EnvOp extends Stmt {
    def name: String
    def expr: Expr
    def isLazy: Boolean
  }
  case class Let(name: String, expr: Expr, isLazy: Boolean) extends EnvOp
  case class Set(name: String, expr: Expr, isLazy: Boolean) extends EnvOp
  sealed trait Expr extends Stmt
  case class Literal(value: Any) extends Expr
  case class Ident(name: String) extends Expr
  case class Call(func: Expr, args: List[Expr]) extends Expr
  case class Lambda(args: List[String], body: Expr) extends Expr
  case class Block(stmts: List[Stmt]) extends Expr

}
class Interpreter {

  import InterpreterAst._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): Any = {
    expr match {
      case i: Ident       => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found")).value
      case Literal(value) => value
      case b: Block       => runAll(env.newScope, b.stmts)._2
      case c: Call =>
        val f = evalExpr(env, c.func)
        f match {
          case f: Function[List[Any], Any] =>
            val resolvedArgs = c.args.map(evalExpr(env, _))
            f(resolvedArgs)
        }
      case l: Lambda => buildFunc(l, env)
    }
  }

  private def buildFunc(d: Lambda, env: Env[RuntimeValue]): Function[List[Any], Any] = (args: List[Any]) => {
    val newEnv = {
      d.args.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, Strict(value))
      }
    }
    evalExpr(newEnv.newScope, d.body)
  }

  def run(env: Env[RuntimeValue], stmt: Ast): (Env[RuntimeValue], Any) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case e: EnvOp =>
        val f: (String, RuntimeValue) => Env[RuntimeValue] = e match {
          case _: Let => env.put
          case _: Set =>
            (name, value) =>
              env.set(name, value)
              env
        }
        val nextEnv = if (!e.isLazy) {
          val evaluated = evalExpr(env, e.expr)
          f(e.name, Strict(evaluated))
        } else {
          f(e.name, Lazy(() => evalExpr(env, e.expr)))
        }
        (nextEnv, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[Ast]): (Env[RuntimeValue], Any) = {
    stmts
      .foldLeft((env, UnitVal: Any)) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt)
      }
  }

}
