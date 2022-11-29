package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._
import com.mistlang.lang.Types.BasicFuncType

import java.util.UUID

object Interpreter {

  import IR._

  def evalExpr(env: Env[RuntimeValue], expr: Expr): RuntimeValue = {
    expr match {
      case i: Ident          => env.get(i.name).getOrElse(throw new RuntimeException(s"${i.name} not found"))
      case IR.IntLiteral(i)  => IntVal(i)
      case IR.StrLiteral(s)  => StrVal(s)
      case IR.BoolLiteral(b) => BoolVal(b)
      case record: IR.Record =>
        RuntimeValue.Dict(
          record.rows.map(r => r.key -> evalExpr(env, r.value)).toMap
        )
      case l: Lambda =>
        Func((args: List[RuntimeValue]) => {
          val newEnv =
            BasicFuncType.getArgs(l.tpe).map(_._1).zip(args).foldLeft(env.newScope) { case (curEnv, (name, value)) =>
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

sealed trait RuntimeValue {
  def getInt: Int = throw new RuntimeException(s"$this is not a Int")
  def getString: String = throw new RuntimeException(s"$this is not a String")
  def getBoolean: Boolean = throw new RuntimeException(s"$this is not a Boolean")
  def getDict: Dict = throw new RuntimeException(s"$this is not a Dict")
  def getFunc: Func = throw new RuntimeException(s"$this is not a Function")
}
object RuntimeValue {
  sealed trait Primitive extends RuntimeValue
  case class StrVal(value: String) extends Primitive {
    override def getString: String = value
  }
  case class BoolVal(value: Boolean) extends Primitive {
    override def getBoolean: Boolean = value
  }
  case class IntVal(value: Int) extends Primitive {
    override def getInt: Int = value
  }
  case class SymbolVal(uuid: UUID) extends Primitive
  object SymbolVal {
    def apply() = new SymbolVal(UUID.randomUUID())
  }

  case object UnitVal extends Primitive
  case object Null extends Primitive
  case class Dict(fields: Map[String, RuntimeValue]) extends RuntimeValue {
    def apply(s: String): RuntimeValue = fields(s)
    def +(pair: (String, RuntimeValue)*): Dict = Dict(fields ++ pair)

    override def getDict: Dict = this
  }
  object Dict {
    def apply(values: (String, RuntimeValue)*): Dict = new Dict(values.toMap)
  }
  case class Func(f: List[RuntimeValue] => RuntimeValue) extends RuntimeValue {
    override def getFunc: Func = this
  }

}

object Types {

  val IntType = Primitive("int")
  val StrType = Primitive("string")
  val BoolType = Primitive("bool")
  val UnitType = Primitive("unit")
  val RecordType = Primitive("record")
  val FuncType = Primitive("function")
  val AnyPrimitive = Dict()

  private def Primitive(name: String): Dict = Dict("tpe" -> StrVal(name), "hash" -> SymbolVal())
  def IntLiteralType(i: Int): Dict = IntType + ("value" -> IntVal(i))
  def StringLiteralType(s: String): Dict = StrType + ("value" -> StrVal(s))
  def BoolLiteralType(b: Boolean): Dict = BoolType + ("value" -> BoolVal(b))
  def RecordType(values: (String, Dict)*): Dict =
    Dict("tpe" -> RecordType, "fields" -> Dict(values: _*))

  object BasicFuncType {
    def apply(args: List[(String, RuntimeValue)], out: RuntimeValue, isLambda: Boolean): Dict = {
      val f = Func((actualArgs: List[RuntimeValue]) => {
        if (actualArgs.length != args.length)
          Typer.error(s"Unexpected number of args - expected ${args.length}, got ${actualArgs.length}")

        args.zip(actualArgs).foreach { case ((name, e), a) =>
          Typer.validateType(e, a, name)
        }
        out
      })
      FuncType + (List(
        "args" -> Dict(args.toMap),
        "indexes" -> Dict(args.zipWithIndex.map { case ((name, _), idx) =>
          name -> IntVal(idx)
        }.toMap),
        "out" -> out,
        "isLambda" -> BoolVal(isLambda),
        "f" -> f
      ): _*)
    }

    def getArgs(r: Dict): List[(String, RuntimeValue)] = {
      val indexes = r("indexes").getDict.fields.map { case (key, idx) => key -> idx.getInt }.toList.sortBy(_._2)
      val args = r("args").getDict
      indexes.map { case (key, _) => key -> args(key) }
    }

    def op(a: Dict, b: Dict, out: Dict): Dict = {
      apply(List(("a", a), ("b", b)), out, isLambda = false)
    }
  }

  def intersect(left: Dict, right: Dict): Dict = {
    val keys = left.fields.keySet.intersect(right.fields.keySet)
    val matching = keys
      .map { key =>
        val leftVal = left(key)
        val rightVal = right(key)
        (leftVal, rightVal) match {
          case (ld: Dict, rd: Dict) => Some(key -> intersect(ld, rd))
          case (l, r) if l == r     => Some(key -> l)
          case _                    => None
        }
      }
      .collect { case Some(pair) => pair }
      .toMap
    Dict(matching)
  }

}
