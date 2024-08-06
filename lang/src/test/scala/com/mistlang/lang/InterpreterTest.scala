package com.mistlang.lang

object InterpreterTest extends App {
  def runProgram(p: Ast.Program): Any = {
    TypeInterpreter.typeStmts(p)
    EvaluatingInterpreter.run(p) match {
      case InterpreterValue.PrimitiveValue(value) => value
      case InterpreterValue.Dict(m)               => m
      case InterpreterValue.Func(f)               => ???
      case InterpreterValue.UnitValue             => "Unit"
    }
  }

  val runner = new RuntimeTestRunner(runProgram)
  runner.runTests()
  //runner.runTest("tests/well_typed/namespaces.mist")

}
