package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import IR._

  val nullVal = "Null"

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case Set(name, newValue) =>
        env.set(name, evalExpr(env, newValue))
        UnitVal
      case IR.IntLiteral(i)  => IntVal(i)
      case IR.StrLiteral(s)  => StrVal(s)
      case IR.BoolLiteral(b) => BoolVal(b)
      case l: Lambda =>
        Func((args: List[RuntimeValue]) => {
          val newEnv = l.tpe.expected.map(_._1).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
            curEnv.put(name, value)
          }
          runAll(newEnv, l.body)._2
        })
      case c: Call =>
        val f = evalExpr(env, c.expr)
        f match {
          case f: Func =>
            val resolvedArgs = c.args.map(evalExpr(env, _))
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
  case object Null extends Primitive
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue

  sealed trait Type extends RuntimeValue
  object Type {
    case object IntType extends Type
    case object StrType extends Type
    case object BoolType extends Type
    case object UnitType extends Type
    case object AnyType extends Type

    sealed trait FuncType extends Type {
      val f: List[Type] => Type
    }

    case class BasicFuncType(expected: List[(String, Type)], out: Type) extends FuncType {
      override val f: List[Type] => Type = (args: List[Type]) => {
        if (args.length != expected.length)
          Typer.error(s"Unexpected number of args - expected ${expected.length}, got ${args.length}")
        expected.zip(args).foreach { case ((name, e), a) =>
          Typer.checkType(e, a, name)
        }
        out
      }
    }

    object BasicFuncType {
      def op(a: Type, b: Type, out: Type): BasicFuncType = {
        BasicFuncType(List(("a", a), ("b", b)), out)
      }
    }
    case class TypelevelFunc(f: List[Type] => Type) extends FuncType
  }

}
