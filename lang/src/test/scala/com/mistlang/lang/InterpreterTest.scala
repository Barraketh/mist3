package com.mistlang.lang

import com.mistlang.lang.FastparseParser.IdProvider

object InterpreterTest extends App {
  def runProgram(p: Ast.Program, idProvider: IdProvider): Any = {
    val flatProgram = FlattenProgram(p)

    val res = TypeInterpreter.typeStmts(flatProgram).value
    res.map {
      case ComptimeValue.SimpleValue(value) => value
      case ComptimeValue.Dict(m)            => m
    }.get
  }

  val runner = new RuntimeTestRunner(runProgram)
  runner.runTests()
  //runner.runTest("tests/well_typed/blocks.mist")

}
