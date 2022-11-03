package com.mistlang.lang

import com.mistlang.java.codegen.CodeGenerator
import utest._

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLClassLoader
import javax.tools.ToolProvider

object JavaCodeGeneratorTest extends TestSuite {
  val parser = FastparseParser
  val typer = Typer
  val gen = CodeGenerator

  val compiler = ToolProvider.getSystemJavaCompiler

  def run(s: String) = {
    val e = parser.parse(s)
    val typed = typer.eval(TypeEnv.env, e)
    val c = gen.compile(typed, Nil, "MyClass")
    println(c)

    val writer = new BufferedWriter(new FileWriter("test/MyClass.java"))
    writer.write(c)
    writer.close()

    compiler.run(null, null, null, "test/MyClass.java")

    val testDir = new File("/Users/ptsier/projects/mist3/test")
    val classLoader = URLClassLoader.newInstance(Array(testDir.toURI.toURL))

    val cls = Class.forName("MyClass", true, classLoader)
    val instance = cls.getDeclaredConstructors.apply(0).newInstance()
    cls.getDeclaredMethods.toList.find(_.getName == "run").get.invoke(instance)
  }

  val tests = Tests {
    test("Test arithm") {
      val s = "2 * 6 + 3"
      val res = run(s)
      assert(res == 15)
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
      val code =
        s"""
       def fib = fn(n: Int): Int => {
         if (n == 0) 0
         else if (n == 1) 1
         else fib(n - 1) + fib(n - 2)
       }

       fib(6)
     """

      assert(run(code) == 8)
    }
    test("Tuples") {
      assert(
        run(
          """
             def add3 = fn (a: Int) => a + 3
            val a = #[ "3", "5", 6 ]
            val idx0 = 1
            val index = idx0 + 1
            add3(at(a, index))
          """
        ) == 9
      )
    }
    test("Function References") {
      assert(
        run(
          """
            |def add3 = fn (i: Int) => i + 3
            |
            |val a = add3
            |a(3)
            |""".stripMargin
        ) == 6
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
      """) == "baz"
      )
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
        ) == 7
      )
    }
    test("Inline functions") {
      assert(
        run(
          """
            |def add3 = inline (a: Int) => a + 3
            |
            |val x = add3(6)
            |x
            |""".stripMargin
        ) == 9
      )
    }
  }

}
