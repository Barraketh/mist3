package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue._
import com.mistlang.interpreter.{Env, Interpreter, RuntimeValue, InterpreterAst => IA}
import com.mistlang.lang.Ast.TopLevelStmt

object MistInterpreter {
  private def compile(e: Ast.Expr): IA.Expr[Any] = e match {
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

  private def compileTopLevel(s: TopLevelStmt): IA.Stmt[Any] = {
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
    IA.Set(s.name, res)
  }

  private def compileTopLevel(stmts: List[TopLevelStmt]): List[IA.Stmt[Any]] = {
    val nameExprs = stmts.map(s => IA.Let(s.name, IA.Literal(null)))
    val topLevelStmts = stmts.map(compileTopLevel)
    nameExprs ::: topLevelStmts
  }

  private def compileStmt(s: Ast.Stmt): IA.Stmt[Any] = s match {
    case Ast.Val(name, expr) => IA.Let(name, compile(expr))
    case expr: Ast.Expr      => compile(expr)
  }
  def compile(p: Ast.Program): List[IA.Ast[Any]] = {
    val topLevel = compileTopLevel(p.topLevelStmts)
    val body = p.stmts.map(compileStmt)
    topLevel ::: body
  }

  val intrinsics: Map[String, RuntimeValue[Any]] = Map(
    "+" -> Func[Any] { case (a: Value[Int]) :: (b: Value[Int]) :: Nil => Strict(a.value + b.value) },
    "-" -> Func[Any] { case (a: Value[Int]) :: (b: Value[Int]) :: Nil => Strict(a.value - b.value) },
    "*" -> Func[Any] { case (a: Value[Int]) :: (b: Value[Int]) :: Nil => Strict(a.value * b.value) },
    "==" -> Func[Any] { case (a: Value[Any]) :: (b: Value[Any]) :: Nil => Strict(a.value == b.value) },
    "Unit" -> UnitVal,
    "if" -> Func[Any] { case (cond: Value[Boolean]) :: (success: Func[Any]) :: (failure: Func[Any]) :: Nil =>
      if (cond.value) success.f(Nil) else failure.f(Nil)
    },
    "Object" -> Func[Any] { args =>
      val obj = args
        .grouped(2)
        .map { case (key: Value[String]) :: value :: Nil =>
          key.value -> value
        }
        .toMap

      Strict(obj)
    },
    "getMember" -> Func[Any] { case (map: Value[Map[String, RuntimeValue[Any]]]) :: (key: Value[String]) :: Nil =>
      map.value(key.value)
    }
  )

  val runtimeEnv = Env.make(
    intrinsics.map { case (name, v) => name -> v },
    None
  )

  val interpreter = new Interpreter[Any]

  def run(p: Ast.Program): Any = {
    interpreter.runAll(runtimeEnv, compile(p))._2 match {
      case a: Value[Any] => a.value
    }
  }

}
