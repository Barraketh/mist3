package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import IR._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident          => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case IR.IntLiteral(i)  => IntVal(i)
      case IR.StrLiteral(s)  => StrVal(s)
      case IR.BoolLiteral(b) => BoolVal(b)
      case record: IR.Record =>
        RuntimeValue.Record(
          record.rows.map(r => r.key -> evalExpr(env, r.value)).toMap
        )
      case l: Lambda =>
        Func((args: List[RuntimeValue]) => {
          val newEnv = l.tpe.args.map(_._1).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
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
      case i: IR.If =>
        val cond = evalExpr(env, i.expr)
        cond match {
          case BoolVal(true)  => evalExpr(env, i.success)
          case BoolVal(false) => evalExpr(env, i.fail)
        }
    }
  }

  def run(env: Env[RuntimeValue], stmt: IR): (Env[RuntimeValue], RuntimeValue) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case Let(name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated), UnitVal)
      case d: Def =>
        val evaluated = evalExpr(env, d.l)
        env.set(d.name, evaluated)
        (env, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[IR]): (Env[RuntimeValue], RuntimeValue) = {
    val startEnv = stmts.foldLeft(env) { case (curEnv, nextStmt) =>
      nextStmt match {
        case d: Def => curEnv.put(d.name, Null, mutable = true)
        case _      => curEnv
      }
    }
    stmts.foldLeft((startEnv, UnitVal: RuntimeValue)) { case ((curEnv, _), nextStmt) =>
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
  case class Record(value: Map[String, RuntimeValue]) extends RuntimeValue
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

    case class RecordType(rows: Map[String, Type]) extends Type

    sealed trait FuncType extends Type {
      val f: List[Type] => Type
    }

    case class BasicFuncType(args: List[(String, Type)], out: Type, isLambda: Boolean) extends FuncType {
      override val f: List[Type] => Type = (actualArgs: List[Type]) => {
        if (actualArgs.length != args.length)
          Typer.error(s"Unexpected number of args - expected ${args.length}, got ${actualArgs.length}")
        args.zip(actualArgs).foreach { case ((name, e), a) =>
          Typer.checkType(e, a, name)
        }
        out
      }
    }

    object BasicFuncType {
      def op(a: Type, b: Type, out: Type): BasicFuncType = {
        BasicFuncType(List(("a", a), ("b", b)), out, isLambda = false)
      }
    }
    case class TypelevelFunc(f: List[Type] => Type) extends FuncType
  }

}
