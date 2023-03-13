package com.mistlang.lang

import utest._

object InterpreterTest extends TestSuite {
  private val parser = FastparseParser

  def run(s: String): Any = {
    val p = parser.parse(s)
    MistInterpreter.run(p)
  }

  val tests = Tests {
    test("Test arithm") {
      val s = "3 * 2 + 3"
      val res = run(s)
      assert(res == 9)
    }

    test("Recursion") {
      assert(run {
        """
          def fib(n : Int): Int = {
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
