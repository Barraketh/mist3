package com.mistlang.lang

import java.nio.file.{FileSystems, Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters._

class RuntimeTestRunner(runProgram: Ast.Program => Any) {
  def renderRes(res: Any): String = {
    res match {
      case s: String => "\"" + s + "\""
      case other => other.toString
    }
  }

  def runTest(path: Path): Unit = {
    println(path)
    val src = Source.fromFile(path.toFile)
    val lines = src.getLines().toList
    src.close()

    val expected = lines.head
    val programText = lines.tail.mkString("\n")
    val p = FastparseParser.parse(programText)
    val res = renderRes(runProgram(p))
    assert(res == expected, s"Expected $expected, got $res")

  }

  def runTests(): Unit = {
    val testFiles = Files.list(FileSystems.getDefault.getPath("tests", "well_typed")).iterator().asScala
    testFiles.foreach(runTest)
  }

}
