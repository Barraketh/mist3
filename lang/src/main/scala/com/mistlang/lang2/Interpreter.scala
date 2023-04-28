package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue._
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang2.Ast._

object Interpreter {

  object EvaluatorVisitor extends Visitor[Any] {
    override def unit: Any = UnitVal
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

    override def as(a: As, env: Env[RuntimeValue]): Any = evalExpr(a.expr, env)

    override def cache(id: Int, t: Any): Unit = ()
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

    def f3[A, B, C](f: (A, B, C) => Any): Func = l => {
      assert(l.length == 3)
      f(l(0).asInstanceOf[A], l(1).asInstanceOf[B], l(2).asInstanceOf[C])
    }

    val intrinsics: Map[String, Any] = Map(
      "+" -> f2[Int, Int]((a, b) => a + b),
      "-" -> f2[Int, Int]((a, b) => a - b),
      "*" -> f2[Int, Int]((a, b) => a * b),
      "==" -> f2[Any, Any]((a, b) => a == b),
      "Bool" -> f1[String](s => s.toBoolean),
      "Int" -> f1[String](s => s.toInt),
      "Unit" -> UnitVal,
      "if" -> f3[Boolean, Func, Func]((cond, success, failure) => if (cond) success(Nil) else failure(Nil)),
      "Object" -> { (args: List[Any]) =>
        val obj = args
          .grouped(2)
          .map { case (key: String) :: value :: Nil =>
            key -> value
          }
          .toMap

        obj
      },
      "getMember" -> f2[Map[String, Any], String]((map, key) => map(key))
    )

    Env.make(
      intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
      None
    )
  }

  def evalExpr(e: Expr, env: Env[RuntimeValue]): Any = EvaluatorVisitor.evalExpr(e, env)

  def run(stmts: List[Stmt]): Any = Evaluator.runAll(stdEnv, stmts, EvaluatorVisitor)._2

}
