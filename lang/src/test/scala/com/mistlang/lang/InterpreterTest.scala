package com.mistlang.lang

import com.mistlang.lang2.{Interpreter, Typer}

class InterpreterTest extends RuntimeTests {
  def runProgram(p: Ast.Program): Any = {
    Typer.typeStmts(p.topLevelStmts ::: p.stmts)
    Interpreter.run(p.topLevelStmts ::: p.stmts)
  }

}
