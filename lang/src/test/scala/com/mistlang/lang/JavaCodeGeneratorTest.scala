//package com.mistlang.lang
//
//import com.mistlang.java.codegen.{CodeGenerator, JavaCompiler}
//
//import java.io.{BufferedWriter, File, FileWriter}
//import java.net.URLClassLoader
//import javax.tools.ToolProvider
//
//class JavaCodeGeneratorTest extends RuntimeTests {
//  val gen = CodeGenerator
//
//  val compiler = ToolProvider.getSystemJavaCompiler
//
//  def runProgram(p: Ast.Program): Any = {
//    val typed = Typer.compile(p)
//    val c = gen.compile(JavaCompiler.compile(typed), Nil, "MyClass")
//
//    val writer = new BufferedWriter(new FileWriter("test/MyClass.java"))
//    writer.write(c)
//    writer.close()
//
//    compiler.run(null, null, null, "-cp", "lang/target/scala-2.13/classes", "test/MyClass.java")
//
//    val testDir = new File("test")
//    val classpathDir = new File("lang/target/scala-2.13/classes")
//    val classLoader = URLClassLoader.newInstance(Array(testDir.toURI.toURL, classpathDir.toURI.toURL))
//
//    val cls = Class.forName("MyClass", true, classLoader)
//    val runFunc = cls.getField("run")
//    val apply = runFunc.getType.getMethod("apply")
//    apply.invoke(runFunc.get(null))
//  }
//}
