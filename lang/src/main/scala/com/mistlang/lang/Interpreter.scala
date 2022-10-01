package com.mistlang.lang

import com.mistlang.lang.AstOp.{Ast, Expr}
import com.mistlang.lang.Interpreter.{BoolVal, FuncVal, IntVal, RuntimeValue, StrVal, TupleVal, UnitVal}

case class InterpreterError(msg: String) extends Exception

class Interpreter {

  def error(msg: String) = throw InterpreterError(msg)

  private def evalFuncApply(env: Env[RuntimeValue], children: List[Ast]): RuntimeValue = {
    if (children.isEmpty) error("Must have at least one child for function call")
    val fExp = children.head
    val argExps = children.tail

    val f = evalExp(env, fExp)
    f match {
      case f: FuncVal =>
        if (f.numArgs != argExps.length)
          error(s"Unexpected number of args - expected ${f.numArgs}, got ${argExps.length}")

        val resolvedArgs = argExps.map(e => evalExp(env, e))
        f.f(resolvedArgs)
      case other => throw new RuntimeException(s"Cannot do function application with $other")
    }
  }

  private def evalLambda(env: Env[RuntimeValue], l: AstOp.Lambda, children: List[Ast]): RuntimeValue = {
    val body = children.last
    val f = (args: List[RuntimeValue]) => {
      val newEnv = l.argNames.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, Strict(value))
      }
      evalExp(newEnv, body)
    }
    FuncVal(l.argNames.length, f)
  }

  def evalExp(env: Env[RuntimeValue], ast: Ast): RuntimeValue = {
    ast.data match {
      case AstOp.Literal(value) =>
        value match {
          case i: Int     => IntVal(i)
          case b: Boolean => BoolVal(b)
          case s: String  => StrVal(s)
        }
      case AstOp.Tuple => TupleVal(ast.children.map(t => evalExp(env, t)))
      case AstOp.Block => eval(env.newScope, ast.children)
      case AstOp.Ident(name) =>
        env.get(name).map(_.value).getOrElse {
          throw new RuntimeException(s"$name not found")
        }
      case l: AstOp.Lambda => evalLambda(env, l, ast.children)
      case AstOp.Call      => evalFuncApply(env, ast.children)
    }
  }

  def eval(env: Env[RuntimeValue], stmts: List[Ast]): RuntimeValue = {
    var topLevelEnv = env
    stmts.foreach(s =>
      s.data match {
        case d: AstOp.Def =>
          topLevelEnv = topLevelEnv.put(d.name, Lazy(s.children.head, () => topLevelEnv, evalExp))
        case _ => ()
      }
    )
    stmts
      .filter(s =>
        s.data match {
          case _: AstOp.Def => false
          case _            => true
        }
      )
      .foldLeft((topLevelEnv, UnitVal: RuntimeValue)) { case ((curEnv, _), stmt) =>
        stmt.data match {
          case _: Expr         => (curEnv, evalExp(curEnv, stmt))
          case AstOp.Val(name) => (curEnv.put(name, Strict(evalExp(curEnv, stmt.children.head))), UnitVal)
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

  case class FuncVal(numArgs: Int, f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue

  object UnitVal extends RuntimeValue
}
