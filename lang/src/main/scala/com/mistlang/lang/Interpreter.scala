package com.mistlang.lang

import com.mistlang.lang.Ast._
import com.mistlang.lang.RuntimeValue._

case class InterpreterError(msg: String) extends Exception

object Interpreter {
  type RuntimeEnv = Env[ValueHolder[RuntimeValue]]

  def error(msg: String) = throw InterpreterError(msg)

  private def evalFuncApply(env: RuntimeEnv, call: Call): RuntimeValue = {
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

  private def evalFuncData(env: RuntimeEnv, l: FuncData) = { (args: List[RuntimeValue]) =>
    {
      val newEnv = l.args.map(_.name).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, Strict(value))
      }
      evalExp(newEnv, l.body)
    }
  }

  def evalIf(env: RuntimeEnv, i: If): RuntimeValue = {
    val cond = evalExp(env, i.expr)
    cond match {
      case BoolVal(value) =>
        if (value) evalExp(env, i.succ) else evalExp(env, i.fail)
      case other => error(s"Unexpected condition - expected bool, got $other")
    }
  }

  def evalExp(env: RuntimeEnv, ast: Expr): RuntimeValue = {
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
        env.get(name).map(_.value).getOrElse { error(s"$name not found") }
      case c: Call => evalFuncApply(env, c)
      case l: Lambda =>
        FuncVal(l.data.args.length, evalFuncData(env, l.data))
      case i: If => evalIf(env, i)
    }
  }

  def eval(env: RuntimeEnv, stmts: List[Ast]): RuntimeValue = {
    var topLevelEnv = env
    stmts.foreach {
      case d: Ast.Def =>
        topLevelEnv = topLevelEnv.put(d.name, Lazy(Lambda(d.data, Some(d.name)), () => topLevelEnv, evalExp))
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
          case Ast.Val(name, expr) => (curEnv.put(name, Strict(evalExp(curEnv, expr))), UnitVal)
        }
      }
      ._2
  }
}

sealed trait RuntimeValue

object RuntimeValue {
  case class IntVal(value: Int) extends RuntimeValue
  case class BoolVal(value: Boolean) extends RuntimeValue
  case class StrVal(value: String) extends RuntimeValue
  case class TupleVal(arr: List[RuntimeValue]) extends RuntimeValue

  case class FuncVal(numArgs: Int, f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue

  object UnitVal extends RuntimeValue
}
