package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types.BasicFuncTypeR
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

    def isFuncType: Boolean = tpe match {
      case b: BasicFuncTypeR => true
      case _                 => false
    }

    def getFuncType: BasicFuncTypeR = tpe match {
      case b: BasicFuncTypeR => b
      case _                 => Typer.error(s"$this is not a func")
    }
  }

  sealed trait RuntimeType

  object Types {

    case object AnyTypeR extends RuntimeType
    case object IntTypeR extends RuntimeType
    case object StrTypeR extends RuntimeType
    case object BoolTypeR extends RuntimeType
    case object UnitTypeR extends RuntimeType
    case class BasicFuncTypeR(args: List[Type], out: Type) extends RuntimeType

    val AnyType: Type = Type(AnyTypeR)
    val IntType: Type = Type(IntTypeR)
    val BoolType: Type = Type(BoolTypeR)
    val StrType: Type = Type(StrTypeR)
    val UnitType: Type = Type(UnitTypeR)

    def IntLiteralType(i: Int): Type = IntType + ("value" -> IntVal(i))
    def StringLiteralType(s: String): Type = StrType + ("value" -> StrVal(s))
    def BoolLiteralType(b: Boolean): Type = BoolType + ("value" -> BoolVal(b))

    object BasicFuncType {
      def apply(args: List[Type], out: Type): Type = {
        Type(BasicFuncTypeR(args, out))
      }

      def op(a: Type, b: Type, out: Type): Type = {
        apply(List(a, b), out)
      }
    }

  }

}
