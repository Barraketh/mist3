package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types.BasicFuncTypeR
import com.mistlang.lang.RuntimeValue._

object Interpreter {

  import IR._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident          => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case IR.IntLiteral(i)  => IntVal(i)
      case IR.StrLiteral(s)  => StrVal(s)
      case IR.BoolLiteral(b) => BoolVal(b)
      case _: IR.Null        => NullVal
      case l: Lambda =>
        Func((args: List[RuntimeValue]) => {
          val newEnv =
            l.tpe.getFuncType.args.map(_._1).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
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
      case Let(name, expr, mutable) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, evaluated, mutable), UnitVal)
      case s: Set =>
        val evaluated = evalExpr(env, s.expr)
        env.set(s.name, evaluated)
        (env, UnitVal)
    }
  }

  def runAll(env: Env[RuntimeValue], stmts: List[IR]): (Env[RuntimeValue], RuntimeValue) = {
    stmts.foldLeft((env, UnitVal: RuntimeValue)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt)
    }
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
    case class BasicFuncTypeR(args: List[(String, Type)], out: Type) extends RuntimeType

    val AnyType: Type = Type(AnyTypeR)
    val IntType: Type = Type(IntTypeR)
    val BoolType: Type = Type(BoolTypeR)
    val StrType: Type = Type(StrTypeR)
    val UnitType: Type = Type(UnitTypeR)

    def IntLiteralType(i: Int): Type = IntType + ("value" -> IntVal(i))
    def StringLiteralType(s: String): Type = StrType + ("value" -> StrVal(s))
    def BoolLiteralType(b: Boolean): Type = BoolType + ("value" -> BoolVal(b))

    object BasicFuncType {
      def apply(args: List[(String, Type)], out: Type): Type = {
        Type(BasicFuncTypeR(args, out))
      }

      def op(a: Type, b: Type, out: Type): Type = {
        apply(List(("a", a), ("b", b)), out)
      }
    }

  }

}
