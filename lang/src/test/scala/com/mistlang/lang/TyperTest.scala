package com.mistlang.lang

import utest._

object TyperTest extends TestSuite {
  val env = TyperIntrinsics.intrinsics.foldLeft(Env.empty[ValueHolder[Type]]) { case (curEnv, (name, f)) =>
    curEnv.put(name, Strict(f))
  }

  val parser = FastparseParser
  val typer = Typer

  def run(s: String): Type = {
    val e = parser.parse(s)
    typer.eval(env, e).last.tpe
  }

  val tests = Tests {
    test("Intrinsic params") {
      def code(param: String) = s"+($param, 3)"

      assert(run(code("1")) == Type.IntType)
      intercept[TypeError](run(code("\"foo\"")))
    }
    test("Regular function params") {
      def code(param: String) = {
        s"""
            def foo(a: Int, b: String) = a
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
          def fib(n : $inParam)${outParam.map(o => s":$o").getOrElse("")} => {
            if (==(n, 0)) 0
            else if (==(n, 1)) 1
            else +( fib(-(n,1)), fib(-(n,2)) )
          }

          fib(6)
         }"""
      }

      assert(run(code("Int", Some("Int"))) == Type.IntType)
      intercept[TypeError](run(code("Any", Some("Int"))))
      intercept[RuntimeException](run(code("Int", None)))
    }
  }

}
