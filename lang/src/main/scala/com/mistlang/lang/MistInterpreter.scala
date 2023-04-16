package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue._
import com.mistlang.interpreter.{Env, Interpreter, RuntimeValue, InterpreterAst => IA}
import com.mistlang.lang.Ast.TopLevelStmt

object MistInterpreter {
  private def compile(e: Ast.Expr): IA.Expr = e match {
    case Ast.Literal(value)      => IA.Literal(value)
    case Ast.Ident(name)         => IA.Ident(name)
    case Ast.Call(func, args, _) => IA.Call(compile(func), args.map(compile))
    case Ast.If(expr, success, fail) =>
      IA.Call(
        IA.Ident("if"),
        List(compile(expr), IA.Lambda(Nil, compile(success)), IA.Lambda(Nil, compile(fail)))
      )
    case Ast.MemberRef(expr, memberName) =>
      IA.Call(IA.Ident("getMember"), List(compile(expr), IA.Literal(memberName)))
    case Ast.Block(stmts) => IA.Block(stmts.map(compileStmt))
  }

  private def compileTopLevel(s: TopLevelStmt): IA.Stmt = {
    val res = s match {
      case Ast.Def(_, args, _, body) => IA.Lambda(args.map(_.name), compile(body))
      case Ast.Struct(_, args) =>
        IA.Lambda(
          args.map(_.name),
          IA.Call(IA.Ident("Object"), args.flatMap(arg => List(IA.Literal(arg.name), IA.Ident(arg.name))))
        )
      case n: Ast.Namespace =>
        val stmts = compileTopLevel(n.children)
        val returnStmt =
          IA.Call(IA.Ident("Object"), n.children.flatMap(s => List(IA.Literal(s.name), IA.Ident(s.name))))
        IA.Block(stmts :+ returnStmt)
    }
    IA.Set(s.name, res, isLazy = false)
  }

  private def compileTopLevel(stmts: List[TopLevelStmt]): List[IA.Stmt] = {
    val nameExprs = stmts.map(s => IA.Let(s.name, IA.Literal(null), isLazy = false))
    val topLevelStmts = stmts.map(compileTopLevel)
    nameExprs ::: topLevelStmts
  }

  private def compileStmt(s: Ast.Stmt): IA.Stmt = s match {
    case Ast.Val(name, expr) => IA.Let(name, compile(expr), isLazy = false)
    case expr: Ast.Expr      => compile(expr)
  }
  def compile(p: Ast.Program): List[IA.Ast] = {
    val topLevel = compileTopLevel(p.topLevelStmts)
    val body = p.stmts.map(compileStmt)
    topLevel ::: body
  }

  type Func = Function[List[Any], Any]

  def f1[A](f: A => Any): Func = l => {
    assert(l.length == 1)
    f(l.head.asInstanceOf[A])
  }

  def f2[A, B](f: (A, B) => Any): Func = l => {
    assert(l.length == 2)
    f(l.head.asInstanceOf[A], l(1).asInstanceOf[B])
  }

  def f3[A, B, C](f: (A, B, C) => Any): Func = l => {
    assert(l.length == 3)
    f(l(0).asInstanceOf[A], l(1).asInstanceOf[B], l(2).asInstanceOf[C])
  }

  val intrinsics: Map[String, Any] = Map(
    "+" -> f2[Int, Int]((a, b) => a + b),
    "-" -> f2[Int, Int]((a, b) => a - b),
    "*" -> f2[Int, Int]((a, b) => a * b),
    "==" -> f2[Any, Any]((a, b) => a == b),
    "Bool" -> f1[String](s => s.toBoolean),
    "Int" -> f1[String](s => s.toInt),
    "Unit" -> UnitVal,
    "if" -> f3[Boolean, Func, Func]((cond, success, failure) => if (cond) success(Nil) else failure(Nil)),
    "Object" -> { (args: List[Any]) =>
      val obj = args
        .grouped(2)
        .map { case (key: String) :: value :: Nil =>
          key -> value
        }
        .toMap

      obj
    },
    "getMember" -> f2[Map[String, Any], String]((map, key) => map(key))
  )

  val runtimeEnv = Env.make(
    intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
    None
  )

  val interpreter = new Interpreter

  def run(p: Ast.Program): Any = {
    interpreter.runAll(runtimeEnv, compile(p))._2
  }

}
