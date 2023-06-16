package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue.Strict
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._
import com.mistlang.lang.Types._
import com.mistlang.lang.{Type, Types}

object TypeCheck {

  def checkType(expected: Type, actual: Type): Boolean = {
    expected == AnyType || expected == actual
  }
  def validateType(expected: Type, actual: Type, name: String): Unit = {
    if (checkType(expected, actual)) ()
    else Typer.error(s"Failed typecheck for $name. Expected: $expected, actual: $actual")
  }
}

object Typer {
  type TypeCache = collection.mutable.Map[Int, Type]

  case class TypeError(msg: String) extends RuntimeException(msg)

  def error(s: String) = throw TypeError(s)

  private def checkType(expected: Type, actual: Type): Unit = {
    if (!(expected == Types.AnyType || expected == actual)) {
      error(s"Type mismatch: expected ${expected}, got ${actual}")
    }
  }

  class TyperVisitor extends Visitor[Type] {
    val map: TypeCache = collection.mutable.Map.empty
    override def unit: Type = UnitType
    override def evalTopLevel: Boolean = true
    override def asT(a: Any): Type = a match {
      case t: Type => t
      case other   => error(s"$other is not a type")
    }
    override def literal(l: Literal): Type = {
      l.value match {
        case _: Boolean => BoolType
        case _: Int     => IntType
        case _: String  => StrType
        case null       => NullType
      }
    }
    override def call(c: Call, env: Env[RuntimeValue]): Type = {
      val typedF = evalExpr(c.func, env)
      typedF match {
        case FuncType(expectedArgs, out) =>
          if (expectedArgs.length != c.args.length)
            error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${c.args.length}")

          val typedArgs = c.args.map(e => evalExpr(e, env))
          expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
            checkType(expected, actual)
          }
          out
        case StructType(_, _, expectedArgs) =>
          if (expectedArgs.length != c.args.length)
            error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${c.args.length}")

          val typedArgs = c.args.map(e => evalExpr(e, env))
          expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
            checkType(expected._2, actual)
          }
          typedF
        case _ => error(s"Cannot call ${typedF}")
      }
    }
    override def lambda(l: Lambda, env: Env[RuntimeValue]): Type = {
      val inputTypes = l.args.map { arg =>
        asT(Interpreter.evalExpr(arg.tpe, env))
      }
      val expectedOut = l.outType.map(o => asT(Interpreter.evalExpr(o, env)))

      val newEnv = l.args.zip(inputTypes).foldLeft(env.newScope) { case (curEnv, nextArg) =>
        curEnv.put(nextArg._1.name, Strict(nextArg._2))
      }

      val withRec = (l.name, expectedOut) match {
        case (Some(name), Some(value)) => newEnv.put(name, Strict(FuncType(inputTypes, value)))
        case _                         => newEnv
      }

      val actualOut = evalExpr(l.body, withRec)
      expectedOut.foreach { o => checkType(o, actualOut) }

      FuncType(inputTypes, expectedOut.getOrElse(actualOut))
    }

    override def cache(id: Int, t: Type): Unit = map.put(id, t)
    override def `if`(i: If, env: Env[RuntimeValue]): Type = {
      val cond = evalExpr(i.expr, env)
      if (cond != BoolType) throw TypeError("Condition must be boolean")

      val succ = evalExpr(i.success, env)
      val fail = evalExpr(i.fail, env)

      if (succ == fail) succ else AnyType
    }
    override def memberRef(m: MemberRef, env: Env[RuntimeValue]): Type = {
      val ref = evalExpr(m.expr, env)
      ref match {
        case s: StructType =>
          val resType = s.args.find(_._1 == m.memberName).getOrElse(error(s"Member name ${m.memberName} not found"))._2
          resType
        case n: NamespaceType =>
          val resType =
            n.children.find(_._1 == m.memberName).getOrElse(error(s"Member name ${m.memberName} not found"))._2
          resType
        case other => throw TypeError(s"Can not get member of ${other}")
      }
    }
    override def struct(s: Struct, env: Env[RuntimeValue]): Type = {
      val argTypes = s.args.map(a => evalExpr(a.tpe, env))
      StructType(s.name, "", s.args.map(_.name).zip(argTypes))
    }

    override def namespace(n: Namespace, env: Env[RuntimeValue]): Type = {
      NamespaceType(n.children.map { c =>
        c.name -> env.get(c.name).get.value.asInstanceOf[Type]
      }.toMap)
    }
  }

  private val stdEnv = {
    val basicTypes: Map[String, Type] = Map(
      "+" -> (FuncType(List(IntType, IntType), IntType)),
      "-" -> (FuncType(List(IntType, IntType), IntType)),
      "*" -> (FuncType(List(IntType, IntType), IntType)),
      "==" -> (FuncType(List(AnyType, AnyType), BoolType)),
      "Unit" -> (UnitType),
      "Any" -> (AnyType),
      "Int" -> (IntType),
      "String" -> (StrType)
    ).map { case (key, value) =>
      key -> value
    }

    val intrinsics = basicTypes + ("Func" -> ((args: List[Any]) => {
      val tpes = args.collect { case t: Type => t }
      FuncType(tpes.take(tpes.length - 1), tpes.last)
    }))

    Env.make[RuntimeValue](
      intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
      None
    )
  }

  def typeStmts(p: Program): Type = Evaluator.runProgram(stdEnv, p, new TyperVisitor())

}
