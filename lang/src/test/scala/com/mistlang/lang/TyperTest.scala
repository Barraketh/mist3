package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._
import Types._
import utest._

object TyperTest extends TestSuite {
  private val typerEnv = Env(
    TyperIntrinsics.intrinsics.map { case (name, v) =>
      name -> Immutable[RuntimeValue](v)
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

      TypeCheck.validateType(IntType, res, "")
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
      TypeCheck.validateType(IntType, res, "")
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

      TypeCheck.validateType(IntType, run(code("Int", "Int")), "")
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

      assert(run(code("2")) == IntType)
      intercept[TypeError](run(code("\"1\"")))
    }
    test("Return type") {
      def code(param: String) =
        s"""
           |def foo(): Int = $param 
           |""".stripMargin

      assert(run(code("1")) == UnitType)
      intercept[TypeError](run(code("\"1\"")))
    }
  }

}
