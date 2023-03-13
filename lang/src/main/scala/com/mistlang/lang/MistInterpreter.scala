package com.mistlang.lang

import com.mistlang.interpreter.{Interpreter, InterpreterAst => IA}
import com.mistlang.lang.RuntimeValue._

object MistInterpreter {
  private def compile(e: Ast.Expr): IA.Expr[Any] = e match {
    case Ast.Literal(value)      => IA.Literal(value)
    case Ast.Ident(name)         => IA.Ident(name)
    case Ast.Call(func, args, _) => IA.Call(compile(func), args.map(compile))
    case Ast.If(expr, success, fail) =>
      IA.Call(
        IA.Ident("if"),
        List(compile(expr), IA.Lambda(Nil, compile(success) :: Nil), IA.Lambda(Nil, compile(fail) :: Nil))
      )
  }

  private def compileStmt(s: Ast.Stmt): IA.Stmt[Any] = s match {
    case Ast.Val(name, expr) => IA.Let(name, compile(expr))
    case expr: Ast.Expr      => compile(expr)
  }
  def compile(p: Ast.Program): List[IA.Ast[Any]] = {
    val funcNames = p.defs.map(d => IA.Let(d.name, IA.Literal(null)))
    val defs = p.defs.map(d => IA.Set(d.name, IA.Lambda(d.args.map(_.name), d.body.map(compileStmt))))
    val body = p.stmts.map(compileStmt)
    funcNames ::: defs ::: body
  }

  val intrinsics: Map[String, RuntimeValue[Any]] = Map(
    "+" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a + b) },
    "-" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a - b) },
    "*" -> Func[Any] { case Value(a: Int) :: Value(b: Int) :: Nil => Value(a * b) },
    "==" -> Func[Any] { case Value(a) :: Value(b) :: Nil => Value(a == b) },
    "Unit" -> UnitVal,
    "if" -> Func[Any] { case Value(cond: Boolean) :: (success: Func[Any]) :: (failure: Func[Any]) :: Nil =>
      if (cond) success.f(Nil) else failure.f(Nil)
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
