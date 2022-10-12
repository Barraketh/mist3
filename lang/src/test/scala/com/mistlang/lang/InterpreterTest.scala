package com.mistlang.lang

import RuntimeValue._
import utest._

object InterpreterTest extends TestSuite {
  val interpreter = RuntimeInterpreter
  val parser = FastparseParser

  def run(s: String): RuntimeValue = {
    val e = parser.parse(s)
    interpreter.eval(e)
  }

  val tests = Tests {
    test("Test arithm") {
      val s = "+(3, *(2, 6))"
      val res = run(s)
      assert(res == IntVal(15))
    }

    test("Block") {
      assert(
        run(
          """{
            | val a = 3
            | val b = 6
            | +(a, b)
            |}""".stripMargin
        ) == IntVal(9)
      )

      assert(
        run(
          """{
            | val a = 3
            | val b = {
            |   val a = 7
            |   +(a, 3)
            | }
            | +(a, b)
            |}""".stripMargin
        ) == IntVal(13)
      )

      assert(
        run(
          """{
            |val a = 3
            |val b = {
            | +(a, 6)
            |}
            |+(a, b)
            |}""".stripMargin
        ) == IntVal(12)
      )
    }

    test("Lambdas") {
      assert(
        run("""
        val a = "foo"
        val myFunc = {
          val a = "bar"
          {
            val a = "baz"
            fn () => a
          }
        }

        myFunc()
      """) == StrVal("baz")
      )
    }

    test("Recursion") {
      assert(run {
        """{
          def fib(n : Any) => {
            if (==(n, 0)) 0
            else if (==(n, 1)) 1
            else +( fib(-(n,1)), fib(-(n,2)) )
          }

          fib(6)
         }""".stripMargin
      } == IntVal(8))
    }

    test("Tuples") {
      assert(
        run(
          """
          val a = #[ 3, 5, 6 ]
          at(a, 1)
          """.stripMargin
        ) == IntVal(5)
      )
    }
  }
}
