package com.mistlang.lang

class InterpreterTest extends RuntimeTests {
  def runProgram(p: Ast.Program): Any = {
    MistInterpreter.run(p)
  }

}
