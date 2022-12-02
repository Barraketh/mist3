package com.mistlang.lang

import com.mistlang.java.codegen.CodeGenerator
import utest._

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLClassLoader
import javax.tools.ToolProvider

object JavaCodeGeneratorTest extends TestSuite {
  val gen = CodeGenerator

  val compiler = ToolProvider.getSystemJavaCompiler

  private val typerEnv = Env(
    TyperIntrinsics.intrinsics.map { case (name, v) =>
      name -> Immutable[RuntimeValue](v)
    },
    None
  )

  def run(s: String) = {
    val e = FastparseParser.parse(s)
    val typed = Typer.compile(e, typerEnv)
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
      def fib(n : Int): Int = {
        if (n == 0) 0
        else if (n == 1) 1
        else fib(n - 1) + fib(n - 2)
      }

      fib(6)
     """.stripMargin
      } == 8)
    }
    test("Functions as params") {
      assert(run {
        s"""
           |def f(a: Int, b: Int, myFunc: Func(Int, Int, Int)): Int = myFunc(a, b)
           |
           |def f1(a: Int, b: Int): Int = a + b
           |
           |f(3, 6, f1)
           |
           |""".stripMargin
      } == 9)
    }
  }

}
