package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue._
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._
import com.mistlang.lang.Types.NamespaceType
import com.mistlang.lang2.Typer.error

object Interpreter {

  object EvaluatorVisitor extends Visitor[Any] {
    override def unit: Any = UnitVal
    override def evalTopLevel: Boolean = false
    override def asT(a: Any): Any = a
    override def literal(l: Literal): Any = l.value
    override def call(c: Call, env: Env[RuntimeValue]): Any = {
      val f = evalExpr(c.func, env)
      f match {
        case f: Function[List[Any], Any] =>
          val resolvedArgs = c.args.map(evalExpr(_, env))
          f(resolvedArgs)
        case other => throw new RuntimeException(s"Cannot call ${other}")
      }
    }
    override def lambda(l: Lambda, env: Env[RuntimeValue]): Any = { (args: List[Any]) =>
      {
        val newEnv = {
          l.args.zip(args).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
            curEnv.put(arg.name, Strict(value))
          }
        }
        evalExpr(l.body, newEnv.newScope)
      }
    }
    override def cache(id: Int, t: Any): Unit = ()
    override def `if`(i: If, env: Env[RuntimeValue]): Any = {
      val evaluatedCond = evalExpr(i.expr, env).asInstanceOf[Boolean]
      if (evaluatedCond) evalExpr(i.success, env) else evalExpr(i.fail, env)
    }
    override def memberRef(m: MemberRef, env: Env[RuntimeValue]): Any = {
      val from = evalExpr(m.expr, env)
      from match {
        case map: Map[String, Any] => map(m.memberName)
        case n: NamespaceType =>
          n.children.find(_._1 == m.memberName).getOrElse(error(s"${m.memberName} not found"))._2
        case t => throw new RuntimeException(s"Expected Object or Namespace, got ${t.getClass.getName}")
      }
    }
    override def struct(s: Struct, env: Env[RuntimeValue]): Any = { (args: List[Any]) =>
      s.args.map(_.name).zip(args).toMap
    }

    override def namespace(n: Namespace, env: Env[RuntimeValue]): Any = {
      n.children.map { c =>
        c.name -> env.get(c.name).get.value
      }.toMap
    }
  }

  val stdEnv = {
    type Func = Function[List[Any], Any]

    def f1[A](f: A => Any): Func = l => {
      assert(l.length == 1)
      f(l.head.asInstanceOf[A])
    }

    def f2[A, B](f: (A, B) => Any): Func = l => {
      assert(l.length == 2)
      f(l.head.asInstanceOf[A], l(1).asInstanceOf[B])
    }

    val intrinsics: Map[String, Any] = Map(
      "+" -> f2[Int, Int]((a, b) => a + b),
      "-" -> f2[Int, Int]((a, b) => a - b),
      "*" -> f2[Int, Int]((a, b) => a * b),
      "==" -> f2[Any, Any]((a, b) => a == b),
      "Unit" -> UnitVal
    )

    Env.make(
      intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
      None
    )
  }

  def evalExpr(e: Expr, env: Env[RuntimeValue]): Any = EvaluatorVisitor.evalExpr(e, env)

  def run(p: Program): Any = Evaluator.runProgram(stdEnv, p, EvaluatorVisitor)

}
