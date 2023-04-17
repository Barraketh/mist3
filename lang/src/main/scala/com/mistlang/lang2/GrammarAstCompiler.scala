package com.mistlang.lang2

import com.mistlang.lang.{Ast => G}

object GrammarAstCompiler {

  def compile(p: G.Program): List[Ast.Stmt] = {
    var curId = 0
    def nextId() = {
      curId = curId + 1
      curId
    }

    def compile(e: G.Expr): Ast.Expr = e match {
      case G.Literal(value)      => Ast.Literal(nextId(), value)
      case G.Ident(name)         => Ast.Ident(nextId(), name)
      case G.Call(func, args, _) => Ast.Call(nextId(), compile(func), args.map(compile))
      case G.If(expr, success, fail) =>
        Ast.Call(
          nextId(),
          Ast.Ident(nextId(), "if"),
          List(
            compile(expr),
            Ast.Lambda(nextId(), Nil, None, compile(success)),
            Ast.Lambda(nextId(), Nil, None, compile(fail))
          )
        )
      case G.MemberRef(expr, memberName) =>
        Ast.Call(nextId(), Ast.Ident(nextId(), "getMember"), List(compile(expr), Ast.Literal(nextId(), memberName)))
      case G.Block(stmts) => Ast.Block(nextId(), stmts.map(compileStmt))
    }

    def compileArg(arg: G.ArgDecl): Ast.ArgDecl = Ast.ArgDecl(arg.name, compile(arg.tpe))

    def compileTopLevelStmt(s: G.TopLevelStmt): Ast.Stmt = {
      val res = s match {
        case G.Def(_, args, outType, body) =>
          Ast.Lambda(
            nextId(),
            args.map(compileArg),
            Some(compile(outType)),
            compile(body)
          )
        case G.Struct(_, args) =>
          val compiledArgs = args.map(compileArg)
          Ast.As(
            nextId(),
            Ast.Lambda(
              nextId(),
              args.map(compileArg),
              None,
              Ast.Call(
                nextId(),
                Ast.Ident(nextId(), "Object"),
                args.flatMap(arg => List(Ast.Literal(nextId(), arg.name), Ast.Ident(nextId(), arg.name)))
              )
            ),
            Ast.Call(
              nextId(),
              Ast.Ident(nextId(), "StructType"),
              Ast.Literal(nextId(), s.name) :: compiledArgs.flatMap(arg =>
                List(Ast.Literal(nextId(), arg.name), arg.tpe)
              )
            )
          )
        case n: G.Namespace =>
          val stmts = compileTopLevel(n.children)
          val returnStmt =
            Ast.Call(
              nextId(),
              Ast.Ident(nextId(), "Object"),
              n.children.flatMap(s => List(Ast.Literal(nextId(), s.name), Ast.Ident(nextId(), s.name)))
            )
          Ast.Block(nextId(), stmts :+ returnStmt)
      }
      Ast.Set(s.name, res, isLazy = true)
    }

    def compileTopLevel(stmts: List[G.TopLevelStmt]): List[Ast.Stmt] = {
      val nameExprs = stmts.map(s => Ast.Let(s.name, Ast.Literal(nextId(), null), isLazy = false))
      val topLevelStmts = stmts.map(compileTopLevelStmt)
      nameExprs ::: topLevelStmts
    }

    def compileStmt(s: G.Stmt): Ast.Stmt = s match {
      case G.Val(name, expr) => Ast.Let(name, compile(expr), isLazy = false)
      case expr: G.Expr      => compile(expr)
    }

    val topLevel = compileTopLevel(p.topLevelStmts)
    val body = p.stmts.map(compileStmt)
    topLevel ::: body
  }

}
