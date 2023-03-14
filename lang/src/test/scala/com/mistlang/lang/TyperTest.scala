package com.mistlang.lang

import com.mistlang.lang.Types._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

class TyperTest extends AnyFlatSpec with should.Matchers {

  def run(s: String): Type = {
    val e = FastparseParser.parse(s)
    Typer.compile(e).body.lastOption match {
      case Some(e: IR.Expr) => e.tpe
      case _                => UnitType
    }
  }

  "Intrinsic params" should "work correctly" in {
    def code(param: String) = s"3 + $param"

    val res = run(code("1"))

    TypeCheck.validateType(IntType, res, "")
    intercept[TypeError](run(code("\"foo\"")))
  }

  "Regular function params" should "work correctly" in {
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

  "Recursion" should "work correctly" in {
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

  "If" should "work correctly" in {
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

  "Return types" should "typecheck correctly" in {
    def code(param: String) =
      s"""
         |def foo(): Int = $param
         |""".stripMargin

    assert(run(code("1")) == UnitType)
    intercept[TypeError](run(code("\"1\"")))
  }

  "Functions as params" should "typecheck correctly" in {
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

    TypeCheck.validateType(IntType, run(code("f1")), "")
    intercept[TypeError](run(code("f2")))
  }

  "Structs" should "work correctly" in {
    def code1(param: String) = {
      s"""
         |struct Foo(a: Int, b: $param)
         |
         |def f1(foo: Foo): String = foo.b
         |""".stripMargin

    }
    run(code1("String"))
    intercept[TypeError](run(code1("Int")))

    def code2(param: String) = {
      s"""
         |struct Foo(a: Int, b: String)
         |
         |val foo = Foo(3, $param)
         |foo.b
         |""".stripMargin
    }

    TypeCheck.validateType(StrType, run(code2("\"bar\"")), "")
    intercept[TypeError](run(code2("3")))
  }

}
