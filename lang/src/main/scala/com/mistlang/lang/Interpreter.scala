package com.mistlang.lang

import com.mistlang.lang.Ast._
import com.mistlang.lang.Interpreter._

case class InterpreterError(msg: String) extends Exception

class Interpreter {

  def error(msg: String) = throw InterpreterError(msg)

  private def evalFuncApply(env: Env[RuntimeValue], call: Call): RuntimeValue = {
    val f = evalExp(env, call.func)
    f match {
      case f: FuncVal =>
        if (f.numArgs != call.args.length)
          error(s"Unexpected number of args - expected ${f.numArgs}, got ${call.args.length}")

        val resolvedArgs = call.args.map(e => evalExp(env, e))
        f.f(resolvedArgs)
      case other => throw new RuntimeException(s"Cannot do function application with $other")
    }
  }

  private def evalFuncData(env: Env[RuntimeValue], l: FuncData) = { (args: List[RuntimeValue]) =>
    {
      val newEnv = l.args.map(_.name).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, value)
      }
      evalExp(newEnv, l.body)
    }
  }

  def evalExp(env: Env[RuntimeValue], ast: Expr): RuntimeValue = {
    ast match {
      case Literal(value) =>
        value match {
          case i: Int     => IntVal(i)
          case b: Boolean => BoolVal(b)
          case s: String  => StrVal(s)
        }
      case Tuple(children) => TupleVal(children.map(t => evalExp(env, t)))
      case Block(stmts)    => eval(env.newScope, stmts)
      case Ident(name) =>
        env.get(name).getOrElse { error(s"$name not found") }
      case c: Call => evalFuncApply(env, c)
      case l: Lambda =>
        FuncVal(l.data.args.length, () => evalFuncData(env, l.data))
    }
  }

  def eval(env: Env[RuntimeValue], stmts: List[Ast]): RuntimeValue = {
    var topLevelEnv = env
    stmts.foreach {
      case d: Ast.Def =>
        topLevelEnv = topLevelEnv.put(d.name, FuncVal(d.data.args.length, () => evalFuncData(topLevelEnv, d.data)))
      case _ => ()
    }
    stmts
      .filter {
        case _: Ast.Def => false
        case _          => true
      }
      .foldLeft((topLevelEnv, UnitVal: RuntimeValue)) { case ((curEnv, _), stmt) =>
        stmt match {
          case e: Expr             => (curEnv, evalExp(curEnv, e))
          case Ast.Val(name, expr) => (curEnv.put(name, evalExp(curEnv, expr)), UnitVal)
        }
      }
      ._2
  }
}

object Interpreter {
  sealed trait RuntimeValue
  case class IntVal(value: Int) extends RuntimeValue
  case class BoolVal(value: Boolean) extends RuntimeValue
  case class StrVal(value: String) extends RuntimeValue
  case class TupleVal(arr: List[RuntimeValue]) extends RuntimeValue

  case class FuncVal(numArgs: Int, lazyF: () => List[RuntimeValue] => RuntimeValue) extends RuntimeValue {
    lazy val f: List[RuntimeValue] => RuntimeValue = lazyF()
  }

  object UnitVal extends RuntimeValue
}
