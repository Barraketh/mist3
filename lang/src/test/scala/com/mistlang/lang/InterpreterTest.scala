package com.mistlang.lang

import com.mistlang.lang2.{GrammarAstCompiler, Interpreter, Typer}

class InterpreterTest extends RuntimeTests {
  def runProgram(p: Ast.Program): Any = {
    val ast = GrammarAstCompiler.compile(p)
    Typer.typeStmts(ast)
    Interpreter.run(ast)
  }

}
