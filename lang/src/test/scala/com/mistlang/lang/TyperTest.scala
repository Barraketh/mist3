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
  }

}
