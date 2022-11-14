package com.mistlang.lang

import utest._

object InterpreterTest extends TestSuite {
  val parser = FastparseParser

  val env = Env(
    RuntimeIntrinsics.intrinsics.map { case (name, v) =>
      name -> Immutable[Any](v)
    },
    None
  )

  def run(s: String): Any = {
    val e = parser.parse(s)
    val ir = AstToIR.compile(e)
    Interpreter.runAll(env, ir)._2
  }

  val tests = Tests {
    test("Test arithm") {
      val s = "3 * 2 + 3"
      val res = run(s)
      assert(res == 9)
    }

    test("Block") {
      assert(
        run(
          """{
            | val a = 3
            | val b = 6
            | a + b
            |}""".stripMargin
        ) == 9
      )

      assert(
        run(
          """{
            | val a = 3
            | val b = {
            |   val a = 7
            |   a + 3
            | }
            | a + b
            |}""".stripMargin
        ) == 13
      )

      assert(
        run(
          """{
            |val a = 3
            |val b = {
            | a + 6
            |}
            |a + b
            |}""".stripMargin
        ) == 12
      )
    }

    test("Recursion") {
      assert(run {
        """
          def fib(n : Any): Any = {
            if (n == 0) 0
            else if (n == 1) 1
            else fib(n - 1) + fib(n - 2)
          }

          fib(6)
         """.stripMargin
      } == 8)
    }


  }
}
