package com.mistlang.lang

import com.mistlang.lang.Ast.{FlatProgram, Program}

object FlattenProgram {
  def apply(p: Program): FlatProgram = {
    val flat = NamespaceResolver.resolveNames(p)
    RemoveShadowing.removeShadowing(flat)
  }

}
