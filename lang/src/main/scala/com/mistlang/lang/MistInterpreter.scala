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
    }
    IA.Set(s.name, res)
  }

  private def compileStmt(s: Ast.Stmt): IA.Stmt[Any] = s match {
    case Ast.Val(name, expr) => IA.Let(name, compile(expr))
    case expr: Ast.Expr      => compile(expr)
  }
  def compile(p: Ast.Program): List[IA.Ast[Any]] = {
    val topLevelNames = p.topLevelStmts.map(_.name) //p.structs.map(_.name) ::: p.defs.map(_.name)
    val nameExprs = topLevelNames.map(name => IA.Let(name, IA.Literal(null)))
    val topLevelStmts = p.topLevelStmts.map(compileTopLevel)
    val body = p.stmts.map(compileStmt)
    nameExprs ::: topLevelStmts ::: body
  }

  val intrinsics: Map[String, RuntimeValue[Any]] = Map(
    "+" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a + b) },
    "-" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a - b) },
    "*" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a * b) },
    "==" -> Func[Any] { case Value(a) :: Value(b) :: Nil => Value(a == b) },
    "Unit" -> UnitVal,
    "if" -> Func[Any] { case Value(cond: Boolean) :: (success: Func[Any]) :: (failure: Func[Any]) :: Nil =>
      if (cond) success.f(Nil) else failure.f(Nil)
    },
    "Object" -> Func[Any] { args =>
      val obj = args
        .grouped(2)
        .map { case Value(key: String) :: value :: Nil =>
          key -> value
        }
        .toMap

      Value(obj)
    },
    "getMember" -> Func[Any] { case Value(map: Map[String, RuntimeValue[Any]]) :: Value(key: String) :: Nil =>
      map(key)
    }
  )

  val runtimeEnv = Env.make(
    intrinsics.map { case (name, v) => name -> v },
    None
  )

  val interpreter = new Interpreter[Any]

  def run(p: Ast.Program): Any = {
    interpreter.runAll(runtimeEnv, compile(p)) match {
      case Value(a) => a
    }
  }

}
