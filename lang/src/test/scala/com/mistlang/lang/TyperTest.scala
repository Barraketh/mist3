package com.mistlang.lang

import com.mistlang.lang.Typer.TypeError

import java.nio.file.{FileSystems, Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters._

object TyperTest extends App {

  def run(s: String): Type = {
    val e = FastparseParser.parse(s)
    Typer.typeStmts(e)
  }

  def runTest(path: Path): Unit = {
    println(path)
    val src = Source.fromFile(path.toFile)
    val s = src.getLines().mkString("\n")
    src.close()

    val e = FastparseParser.parse(s)
    try {
      Typer.typeStmts(e)
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
