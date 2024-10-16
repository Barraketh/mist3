package com.mistlang.lang

import com.mistlang.java.codegen.{CodeGenerator, JavaCompiler}
import com.mistlang.lang.FastparseParser.IdProvider

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URLClassLoader
import javax.tools.ToolProvider

object JavaCodeGeneratorTest extends App {
  val gen = CodeGenerator
  val scratchDir = "scratch_dir"

  val compiler = ToolProvider.getSystemJavaCompiler

  def runProgram(p: Ast.Program, idProvider: IdProvider): Any = {
    val javaCompiler = new JavaCompiler
    val flatProgram = FlattenProgram(p)
    val c = gen.compile(javaCompiler.compile(flatProgram, idProvider), Nil, "MyClass")

    val testDir = new File(scratchDir)
    testDir.listFiles().toList.foreach(f => f.delete())

    val writer = new BufferedWriter(new FileWriter(s"$scratchDir/MyClass.java"))
    writer.write(c)
    writer.close()

    compiler.run(null, null, null, "-cp", "lang/target/scala-2.13/classes", s"$scratchDir/MyClass.java")

    val classpathDir = new File("lang/target/scala-2.13/classes")

    val classLoader = URLClassLoader.newInstance(Array(testDir.toURI.toURL, classpathDir.toURI.toURL))

    val cls = Class.forName("MyClass", true, classLoader)
    val runFunc = cls.getField("run")
    val apply = runFunc.getType.getMethod("apply")
    apply.invoke(runFunc.get(null))
  }

  val runner = new RuntimeTestRunner(runProgram)
  //runner.runTests()
  runner.runTest("tests/well_typed/generic_arrays.mist")
}
