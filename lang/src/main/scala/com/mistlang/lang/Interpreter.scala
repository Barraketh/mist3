package com.mistlang.lang

import com.mistlang.lang.AstOp.{Ast, Expr}
import com.mistlang.lang.EnvValue._

case class InterpreterError(msg: String) extends Exception

class Interpreter {
  type EV = EnvValue[Any]

  def error(msg: String) = throw InterpreterError(msg)

  private def evalFuncApply(env: Env[EV], children: List[Ast]): EV = {
    if (children.isEmpty) error("Must have at least one child for function call")
    val fExp = children.head
    val argExps = children.tail

    val f = evalExp(env, fExp)
    f match {
      case f: FuncVal[Any] =>
        if (f.numArgs != argExps.length)
          error(s"Unexpected number of args - expected ${f.numArgs}, got ${argExps.length}")

        val resolvedArgs = argExps.map(e => evalExp(env, e))
        f.f(resolvedArgs)
      case other => throw new RuntimeException(s"Cannot do function application with $other")
    }
  }

  private def evalLambda(env: Env[EV], l: AstOp.Lambda, children: List[Ast]): EV = {
    val body = children match {
      case head :: Nil => head
      case other       => error(s"Lambda should have exactly child, got $other")
    }
    val f = (args: List[EV]) => {
      val newEnv = l.argNames.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, Strict(value))
      }
      evalExp(newEnv, body)
    }
    FuncVal(l.argNames.length, f)
  }

  def evalExp(env: Env[EV], ast: Ast): EV = {
    ast.data match {
      case AstOp.Literal(value) => Term(value)
      case AstOp.Tuple          => TupleVal(ast.children.map(t => evalExp(env, t)))
      case AstOp.Block          => eval(env.newScope, ast.children)
      case AstOp.Ident(name) =>
        env.get(name).map(_.value).getOrElse {
          throw new RuntimeException(s"$name not found")
        }
      case l: AstOp.Lambda => evalLambda(env, l, ast.children)
      case AstOp.Call      => evalFuncApply(env, ast.children)
    }
  }

  def eval(env: Env[EV], stmts: List[Ast]): EV = {
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
      .foldLeft((topLevelEnv, UnitVal: EV)) { case ((curEnv, _), stmt) =>
        stmt.data match {
          case _: Expr         => (curEnv, evalExp(curEnv, stmt))
          case AstOp.Val(name) => (curEnv.put(name, Strict(evalExp(curEnv, stmt.children.head))), UnitVal)
        }
      }
      ._2
  }
}
