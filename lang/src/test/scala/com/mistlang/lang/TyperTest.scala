package com.mistlang.lang

import utest._

object TyperTest extends TestSuite {
  val parser = FastparseParser
  val typer = Typer

  def run(s: String): Type = {
    val e = parser.parse(s)
    typer.eval(TypeEnv.env, e).last.tpe.t
  }

  val tests = Tests {
    test("Intrinsic params") {
      def code(param: String) = s"3 + $param"

      assert(run(code("1")) == Type.IntType)
      intercept[TypeError](run(code("\"foo\"")))
    }
    test("Regular function params") {
      def code(param: String) = {
        s"""
            def foo = fn(a: Int, b: String) => a
            foo($param, "s")
          """
      }

      val res = run(code("1"))
      println(res)
      assert(res == Type.IntType)
      intercept[TypeError](run(code("\"1\"")))
    }
    test("Recursion") {
      def code(inParam: String, outParam: Option[String]) = {
        s"""{
          def fib = fn(n : $inParam)${outParam.map(o => s":$o").getOrElse("")} => {
            if (n == 0) 0
            else if (n == 1) 1
            else fib(n - 1) + fib(n - 2)
          }

          fib(6)
         }"""
      }

      assert(run(code("Int", Some("Int"))) == Type.IntType)
      intercept[TypeError](run(code("Any", Some("Int"))))
      intercept[RuntimeException](run(code("Int", None)))
    }
    test("Type tuples") {
      def code(idx: Int): String = {
        s"""
         def foo = fn(a: String) => a
         
         val t = #["foo", 1, "baz"]
         val index = 0 + $idx
         val b = at(t, index)
         foo(b)
         """.stripMargin
      }

      assert(run(code(0)) == Type.StrType)
      intercept[TypeError](run(code(1)))
    }
    test("Expr defs") {
      assert(
        run(
          """
            |def a = b + 3
            |def b = 4
            |
            |a
            |""".stripMargin
        ) == Type.IntType
      )
    }
  }

}
