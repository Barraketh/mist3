package com.mistlang.lang

object InterpreterTest extends App {
  def runProgram(p: Ast.Program): Any = {
    val flatProgram = NamespaceResolver.resolveNames(p)

    val res = TypeInterpreter.typeStmts(flatProgram).value
    res.map {
      case ComptimeValue.SimpleValue(value) => value
      case ComptimeValue.Dict(m)            => m
    }.get
  }

  val runner = new RuntimeTestRunner(runProgram)
  runner.runTests()
  //runner.runTest("tests/well_typed/namespaces.mist")

}
