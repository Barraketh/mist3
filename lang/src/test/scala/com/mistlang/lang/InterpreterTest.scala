package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.IntVal
import utest._

object InterpreterTest extends TestSuite {
  val parser = FastparseParser

  val runtimeEnv = Env(
    RuntimeIntrinsics.intrinsics.map { case (name, v) =>
      name -> Immutable[RuntimeValue](v)
    },
    None
  )

  val typerEnv = Env(
    TyperIntrinsics.intrinsics.map { case (name, v) =>
      name -> Immutable[RuntimeValue](v)
    },
    None
  )

  def run(s: String): RuntimeValue = {
    val e = parser.parse(s)
    Interpreter.runAll(runtimeEnv, e)
  }

  val tests = Tests {
    test("Test arithm") {
      val s = "3 * 2 + 3"
      val res = run(s)
      assert(res == IntVal(9))
    }

    test("Block") {
      assert(
        run(
          """{
            | val a = 3
            | val b = 6
            | a + b
            |}""".stripMargin
        ) == IntVal(9)
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
        ) == IntVal(13)
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
        ) == IntVal(12)
      )
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
      } == IntVal(8))
    }

    test("Tuples") {
      assert(run {
        """
          |val a = Tuple(1, 2, 3)
          |at(a, 1)
          |
          |""".stripMargin
      } == IntVal(2))
    }

  }
}
