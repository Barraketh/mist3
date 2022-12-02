package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types.BasicFuncType
import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import Ast._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case Ast.Literal(value) =>
        value match {
          case i: Int     => IntVal(i)
          case s: String  => StrVal(s)
          case b: Boolean => BoolVal(b)
        }

      case c: Call =>
        val f = evalExpr(env, c.func)
        f match {
          case f: Func =>
            val resolvedArgs = c.args.map(evalExpr(env, _))
            f.f(resolvedArgs)
        }
      case i: If =>
        val cond = evalExpr(env, i.expr)
        cond match {
          case BoolVal(true)  => evalExpr(env, i.success)
          case BoolVal(false) => evalExpr(env, i.fail)
        }
      case b: Block => runAll(env.newScope, b.stmts)
    }
  }

  private def buildFunc(d: Def, env: Env[RuntimeValue]): Func = Func((args: List[RuntimeValue]) => {
    val newEnv =
      d.args.map(_.name).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
        curEnv.put(name, value)
      }
    evalExpr(newEnv, d.body)
  })

  def run(env: Env[RuntimeValue], stmt: Ast): (Env[RuntimeValue], RuntimeValue) = {
    stmt match {
      case e: Expr => (env, evalExpr(env, e))
      case Val(name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated), UnitVal)
      case d: Def =>
        val func = buildFunc(d, env)
        env.set(d.name, func)
        (env, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[Ast]): RuntimeValue = {
    stmts
      .foldLeft((env, UnitVal: RuntimeValue)) { case ((curEnv, _), nextStmt) =>
        run(curEnv, nextStmt)
      }
      ._2
  }

  def runAll(env: Env[RuntimeValue], program: Program): RuntimeValue = {
    val newEnv = program.defs.foldLeft(env) { case (curEnv, d) => curEnv.put(d.name, NullVal, mutable = true) }
    runAll(newEnv, program.defs ::: program.stmts)
  }

}

sealed trait RuntimeValue {
  def getType: Type = {
    this match {
      case t: Type => t
      case _       => Typer.error(s"$this is not a type")
    }
  }
}
object RuntimeValue {
  sealed trait Primitive extends RuntimeValue
  case class StrVal(value: String) extends Primitive
  case class BoolVal(value: Boolean) extends Primitive
  case class IntVal(value: Int) extends Primitive
  case object UnitVal extends Primitive
  case object NullVal extends Primitive
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue
  case class Type(tpe: RuntimeType, data: Map[String, RuntimeValue] = Map.empty) extends RuntimeValue {
    def +(pairs: (String, RuntimeValue)*): Type = copy(data = data ++ pairs)

    def getFuncType: BasicFuncType = tpe match {
      case b: BasicFuncType => b
      case _                => Typer.error(s"$this is not a func")
    }
  }

  sealed trait RuntimeType

  object Types {

    case object AnyType extends RuntimeType
    case object IntType extends RuntimeType
    case object StrType extends RuntimeType
    case object BoolType extends RuntimeType
    case object UnitType extends RuntimeType
    case class BasicFuncType(args: List[Type], out: Type) extends RuntimeType

    val AnyTypeInstance: Type = Type(AnyType)
    val IntTypeInstance: Type = Type(IntType)
    val BoolTypeInstance: Type = Type(BoolType)
    val StrTypeInstance: Type = Type(StrType)
    val UnitTypeInstance: Type = Type(UnitType)

    def IntLiteralType(i: Int): Type = IntTypeInstance + ("value" -> IntVal(i))
    def StringLiteralType(s: String): Type = StrTypeInstance + ("value" -> StrVal(s))
    def BoolLiteralType(b: Boolean): Type = BoolTypeInstance + ("value" -> BoolVal(b))

    def BasicFuncTypeInstance(args: List[Type], out: Type): Type = Type(BasicFuncType(args, out))
    def op(a: RuntimeType, b: RuntimeType, out: RuntimeType): Type =
      BasicFuncTypeInstance(List(Type(a), Type(b)), Type(out))

  }

}
