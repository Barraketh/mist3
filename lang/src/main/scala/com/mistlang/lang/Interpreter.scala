package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import IR._

  val unitVal = "Unit"

  private def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case Ident(name) => env.get(name).getOrElse(throw new RuntimeException(s"$name not found"))
      case Set(name, newValue) =>
        env.set(name, evalExpr(env, newValue))
        UnitVal
      case l: Literal =>
        l.value match {
          case s: String  => StrVal(s)
          case i: Int     => IntVal(i)
          case b: Boolean => BoolVal(b)
        }
      case Lambda(argNames, body) =>
        Func((args: List[RuntimeValue]) => {
          val newEnv = argNames.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
            curEnv.put(name, value)
          }
          runAll(newEnv, body)._2
        })
      case Call(func, args) =>
        val f = evalExpr(env, func)
        f match {
          case f: Func =>
            val resolvedArgs = args.map(evalExpr(env, _))
            f.f(resolvedArgs)
        }
    }
  }

  def run(env: Env[RuntimeValue], stmt: IR): (Env[RuntimeValue], RuntimeValue) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case Let(name, expr, mutable) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated, mutable), UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[IR]): (Env[RuntimeValue], RuntimeValue) = {
    stmts.foldLeft((env, UnitVal: RuntimeValue)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt)
    }
  }

}

sealed trait RuntimeValue
object RuntimeValue {
  sealed trait Primitive extends RuntimeValue
  case class StrVal(value: String) extends Primitive
  case class BoolVal(value: Boolean) extends Primitive
  case class IntVal(value: Int) extends Primitive
  case object UnitVal extends Primitive
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue

}
