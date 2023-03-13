package com.mistlang.lang

import com.mistlang.java.codegen.{CodeGenerator, JavaCompiler}
import utest._

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLClassLoader
import javax.tools.ToolProvider

object JavaCodeGeneratorTest extends TestSuite {
  val gen = CodeGenerator

  val compiler = ToolProvider.getSystemJavaCompiler

  def run(s: String) = {
    val e = FastparseParser.parse(s)
    val typed = Typer.compile(e)
    val c = gen.compile(JavaCompiler.compile(typed), Nil, "MyClass")
    println(c)

    val writer = new BufferedWriter(new FileWriter("test/MyClass.java"))
    writer.write(c)
    writer.close()

    compiler.run(null, null, null, "test/MyClass.java")

    val testDir = new File("/Users/ptsier/projects/mist3/test")
    val classLoader = URLClassLoader.newInstance(Array(testDir.toURI.toURL))

    val cls = Class.forName("MyClass", true, classLoader)
    val runFunc = cls.getField("run")
    val apply = runFunc.getType.getMethod("apply")
    apply.invoke(runFunc.get(null))
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
//
//    test("Tuples") {
//      assert(run {
//        """
//          |val a = Tuple(1, 2, 3)
//          |at(a, 1)
//          |
//          |""".stripMargin
//      } == 2)
//    }
  }

}
