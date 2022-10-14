package com.mistlang.lang

import com.mistlang.lang.Ast._
import com.mistlang.lang.RuntimeValue._

case class InterpreterError(msg: String) extends Exception(msg)

object Interpreter extends {
  type RuntimeEnv = Env[RuntimeValue]

  def error(msg: String) = throw InterpreterError(msg)

  def evalLiteral(l: Literal): RuntimeValue = {
    l.value match {
      case i: Int     => IntVal(i)
      case b: Boolean => BoolVal(b)
      case s: String  => StrVal(s)
    }
  }

  private def evalCall(env: RuntimeEnv, call: Call): RuntimeValue = {
    val f = evalExp(env, call.func)
    f match {
      case f: FuncVal =>
        f.numArgs.foreach { numArgs =>
          if (numArgs != call.args.length)
            error(s"Unexpected number of args - expected ${f.numArgs}, got ${call.args.length}")
        }

        val resolvedArgs = call.args.map(e => evalExp(env, e))
        f.f(resolvedArgs)
      case other => throw new RuntimeException(s"Cannot do function application with $other")
    }
  }

  private def evalLambda(env: RuntimeEnv, l: Lambda): RuntimeValue = {
    FuncVal(
      Some(l.args.length),
      (args: List[RuntimeValue]) => {
        val newEnv = l.args.zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
          curEnv.put(name.name, value)
        }
        evalExp(newEnv, l.body)
      }
    )
  }

  def evalIf(env: RuntimeEnv, i: If): RuntimeValue = {
    val cond = evalExp(env, i.expr)
    cond match {
      case BoolVal(value) =>
        if (value) evalExp(env, i.success) else evalExp(env, i.fail)
      case other => error(s"Unexpected condition - expected bool, got $other")
    }
  }

  private def evalBlock(env: RuntimeEnv, b: Block): RuntimeValue = {
    val newEnv = env.newScope
    val stmts = b.stmts

    val (defs, others) = stmts.partition {
      case _: Def => true
      case _      => false
    }
    val topLevel = defs.collect { case d: Def =>
      d.name -> ((curEnv: RuntimeEnv) => evalExp(curEnv, d.func))
    }
    val topLevelEnv = newEnv.putTopLevel(topLevel)
    others
      .foldLeft(topLevelEnv -> (UnitVal: RuntimeValue)) { case ((curEnv, _), stmt) =>
        stmt match {
          case e: Expr => (curEnv, evalExp(curEnv, e))
          case v: Val =>
            val evaluated = evalExp(curEnv, v.expr)
            (curEnv.put(v.name, evaluated), UnitVal)
        }
      }
      ._2
  }

  def evalExp(env: RuntimeEnv, e: Expr): RuntimeValue = e match {
    case l: Literal      => evalLiteral(l)
    case i: Ident        => env.get(i.name).getOrElse(error(s"${i.name} not found"))
    case l: Lambda       => evalLambda(env, l)
    case c: Call         => evalCall(env, c)
    case b: Block        => evalBlock(env, b)
    case i: If           => evalIf(env, i)
    case Tuple(children) => TupleVal(children.map(t => evalExp(env, t)))

  }
}

object RuntimeInterpreter {

  val env = RuntimeIntrinsics.intrinsics.foldLeft(Env.empty[RuntimeValue]) { case (curEnv, (name, f)) =>
    curEnv.put(name, f)
  }

  def eval(e: List[Ast]): RuntimeValue = Interpreter.evalExp(env, Block(e))

}
