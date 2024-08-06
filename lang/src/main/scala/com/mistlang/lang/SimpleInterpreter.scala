package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._
import com.mistlang.lang.InterpreterValue.{Dict, Func, PrimitiveValue}

abstract class BaseInterpreter[T >: Null] {
  type MyEnvType = Env[RuntimeValue[T]]

  def evalExpr(env: MyEnvType, expr: Ast.Expr): T
  def evalStruct(env: MyEnvType, struct: Struct, path: String): T
  def evalNamespace(env: MyEnvType, namespace: Namespace, path: String): T
  def forceTopLevelEval: Boolean
  def unit: T

  def run(env: MyEnvType, stmt: Stmt): (MyEnvType, T) = {
    stmt match {
      case expr: Expr => (env, evalExpr(env, expr))
      case Ast.Val(name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, Strict(evaluated)), unit)
    }
  }

  def runAll(env: MyEnvType, stmts: List[Stmt]): (MyEnvType, T) = {
    stmts.foldLeft((env, unit)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt)
    }
  }

  protected def runTopLevel(env: MyEnvType, stmt: TopLevelStmt, path: String): Unit = {
    stmt match {
      case d: Def       => env.set(d.name, Lazy(() => evalExpr(env, d.lambda)))
      case s: Struct    => env.set(s.name, Lazy(() => evalStruct(env, s, path)))
      case n: Namespace => env.set(n.name, Lazy(() => evalNamespace(env, n, path)))

    }
  }
  protected def runAllTopLevel(env: MyEnvType, stmts: List[TopLevelStmt], path: String): MyEnvType = {
    val newEnv = stmts.map(_.name).foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }
    stmts.foreach(stmt => runTopLevel(newEnv, stmt, path))
    if (forceTopLevelEval) {
      stmts.foreach(s => newEnv.get(s.name).foreach(_.value))
    }
    newEnv
  }
  def runProgram(env: MyEnvType, p: Ast.Program): T = {
    val nextEnv = runAllTopLevel(env, p.topLevelStmts, "")
    runAll(nextEnv, p.stmts)._2
  }
}

object EvaluatingInterpreter extends BaseInterpreter[InterpreterValue] {
  override def evalExpr(env: MyEnvType, expr: Expr): InterpreterValue = expr match {
    case Ast.Literal(_, value) => PrimitiveValue(value)
    case Ast.Ident(_, name) =>
      env
        .get(name)
        .getOrElse { throw new RuntimeException(s"$name not found") }
        .value
    case Ast.Call(_, func, args, _) =>
      val f = evalExpr(env, func)
      f match {
        case f: InterpreterValue.Func =>
          val resolvedArgs = args.map(e => evalExpr(env, e))
          f.f(resolvedArgs)
        case other => throw new RuntimeException(s"Cannot call $other")
      }
    case Ast.MemberRef(_, expr, memberName) =>
      val from = evalExpr(env, expr)
      from match {
        case d: InterpreterValue.Dict => d.m(memberName)
        case other                    => throw new RuntimeException(s"Cannot get member of $other")
      }
    case Ast.If(_, expr, success, fail) =>
      val cond = evalExpr(env, expr)
      cond match {
        case PrimitiveValue(value: Boolean) =>
          if (value) evalExpr(env, success) else evalExpr(env, fail)
        case other => throw new RuntimeException(s"$other is not a boolean")
      }
    case Ast.Block(_, stmts) => runAll(env.newScope, stmts)._2
    case l: Ast.Lambda =>
      val func = (args: List[InterpreterValue]) => {
        val newEnv = l.args.zip(args).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
          curEnv.put(arg.name, Strict(value))
        }
        evalExpr(newEnv, l.body)
      }
      InterpreterValue.Func(func)

    case Ast.Comptime(id, expr) => ???
  }

  override def evalStruct(env: MyEnvType, s: Struct, path: String): InterpreterValue = {
    Func((args: List[InterpreterValue]) => Dict(s.args.map(_.name).zip(args).toMap))
  }

  override def evalNamespace(env: MyEnvType, n: Namespace, path: String): InterpreterValue = {
    val newPath = if (path.isEmpty) n.name else path + "." + n.name
    val namespaceEnv = runAllTopLevel(env.newScope, n.children, newPath)
    Dict(n.children.map { c =>
      c.name -> namespaceEnv.get(c.name).get.value
    }.toMap)
  }

  override def forceTopLevelEval: Boolean = false
  override def unit: InterpreterValue = InterpreterValue.UnitValue

  val stdEnv = {

    def f2int(f: (Int, Int) => Int): Func = Func(l => {
      assert(l.length == 2)
      PrimitiveValue(
        f(
          l.head.asInstanceOf[PrimitiveValue].value.asInstanceOf[Int],
          l(1).asInstanceOf[PrimitiveValue].value.asInstanceOf[Int]
        )
      )
    })

    val intrinsics: Map[String, InterpreterValue] = Map(
      "+" -> f2int((a, b) => a + b),
      "-" -> f2int((a, b) => a - b),
      "*" -> f2int((a, b) => a * b),
      "==" -> Func(l => {
        assert(l.length == 2)
        PrimitiveValue(l.head == l(1))
      }),
      "Unit" -> InterpreterValue.UnitValue
    )

    Env.make(
      intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue[InterpreterValue]) },
      None
    )
  }

  def run(p: Program): InterpreterValue = runProgram(stdEnv, p)
}
