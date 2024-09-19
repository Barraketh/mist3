package com.mistlang.lang

import com.mistlang.lang.TypeInterpreter.TypeError

import java.nio.file.{FileSystems, Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters._

object TyperTest extends App {

  def run(s: String): Type = {
    val e = FastparseParser.parse(s)
    val flatProgram = FlattenProgram(e)
    TypeInterpreter.typeStmts(flatProgram).tpe
  }

  def runTest(path: Path): Unit = {
    println(path)
    val src = Source.fromFile(path.toFile)
    val s = src.getLines().mkString("\n")
    src.close()

    val e = FastparseParser.parse(s)
    val flatProgram = FlattenProgram(e)
    try {
      TypeInterpreter.typeStmts(flatProgram)
      assert(false, "Ill typed program typechecked")
    } catch {
      case e: TypeError =>
    }
  }

  def runTests(): Unit = {
    val testFiles = Files.list(FileSystems.getDefault.getPath("tests", "ill_typed")).iterator().asScala
    testFiles.foreach(runTest)
  }

  runTests()

}
