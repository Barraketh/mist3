package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.Types._
import com.mistlang.lang.RuntimeValue._
import utest._

object TyperTest extends TestSuite {
  private val typerEnv = Env.make(
    TyperIntrinsics.intrinsics.map { case (name, v) =>
      name -> v
    },
    None
  )

  def run(s: String): Type = {
    val e = FastparseParser.parse(s)
    Typer.compile(e, typerEnv).last.tpe
  }

  val tests = Tests {
    test("Intrinsic params") {
      def code(param: String) = s"3 + $param"

      val res = run(code("1"))

      TypeCheck.validateType(IntTypeInstance, res, "")
      intercept[TypeError](run(code("\"foo\"")))
    }
    test("Regular function params") {
      def code(param: String) = {
        s"""
            def foo(a: Int, b: String): Int = a
            foo($param, "s")
          """
      }

      val res = run(code("1"))
      TypeCheck.validateType(IntTypeInstance, res, "")
      intercept[TypeError](run(code("\"1\"")))
    }
    test("Recursion") {
      def code(inParam: String, outParam: String) = {
        s"""
          def fib(n : $inParam)${s":$outParam"} = {
            if (n == 0) 0
            else if (n == 1) 1
            else fib(n - 1) + fib(n - 2)
          }

          fib(6)
         """
      }

      TypeCheck.validateType(IntTypeInstance, run(code("Int", "Int")), "")
      intercept[TypeError](run(code("Any", "Int")))
    }
    test("If") {
      def code(param: String) = {
        s"""
           |def foo(a: Int): Int = a
           |
           |val x = if (true) 1 else $param
           |foo(x)
           |""".stripMargin
      }

      assert(run(code("2")) == IntTypeInstance)
      intercept[TypeError](run(code("\"1\"")))
    }
    test("Return type") {
      def code(param: String) =
        s"""
           |def foo(): Int = $param
           |""".stripMargin

      assert(run(code("1")) == UnitTypeInstance)
      intercept[TypeError](run(code("\"1\"")))
    }
    test("Functions as params") {
      def code(param: String) = {
        s"""
           |def f(a: Int, b: Int, myFunc: Func(Int, Int, Int)): Int = myFunc(a, b)
           |
           |def f1(a: Int, b: Int): Int = a + b
           |def f2(a: Int, b: String): Int = a
           |
           |f(3, 6, $param)
           |
           |""".stripMargin
      }

      TypeCheck.validateType(IntTypeInstance, run(code("f1")), "")
      intercept[TypeError](run(code("f2")))
    }

    test("Type tuples") {
      def code(idx: Int): String = {
        s"""
         def foo(a: String): String = a

         val t = Tuple("foo", 1, "baz")
         val b = at(t, $idx)
         foo(b)
        """.stripMargin
      }

      assert(run(code(0)).tpe == StrType)
      intercept[TypeError](run(code(1)))
    }
  }

}
