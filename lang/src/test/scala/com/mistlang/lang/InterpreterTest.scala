package com.mistlang.lang

import com.mistlang.lang2.{GrammarAstCompiler, Interpreter}

class InterpreterTest extends RuntimeTests {
  def runProgram(p: Ast.Program): Any = {
    Interpreter.run(GrammarAstCompiler.compile(p))
  }

}
