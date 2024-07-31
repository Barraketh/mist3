package com.mistlang.lang

object InterpreterTest extends App {
  def runProgram(p: Ast.Program): Any = {
    Typer.typeStmts(p)
    Interpreter.run(p)
  }

  val runner = new RuntimeTestRunner(runProgram)
  runner.runTests()

}
