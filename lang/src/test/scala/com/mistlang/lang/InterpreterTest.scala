package com.mistlang.lang

object InterpreterTest extends App {
  def runProgram(p: Ast.Program): Any = {
    val res = TypeInterpreter.typeStmts(p).value
    res.map {
      case ComptimeValue.SimpleValue(value) => value
      case ComptimeValue.Dict(m)            => m
    }.get
  }

  val runner = new RuntimeTestRunner(runProgram)
  runner.runTests()
  //runner.runTest("tests/well_typed/recursion.mist")

}
