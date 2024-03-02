package com.mistlang.lang

class InterpreterTest extends RuntimeTests {
  def runProgram(p: Ast.Program): Any = {
    Typer.typeStmts(p)
    Interpreter.run(p)
  }

}
