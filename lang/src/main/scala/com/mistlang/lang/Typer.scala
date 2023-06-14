package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.Strict
import com.mistlang.interpreter.{Env, Interpreter, RuntimeValue, InterpreterAst => IA}
import com.mistlang.lang.Ast.Block
import com.mistlang.lang.Types._

object TypeCheck {

  def checkType(expected: Type, actual: Type): Boolean = {
    expected == AnyType || expected == actual
  }
  def validateType(expected: Type, actual: Type, name: String): Unit = {
    if (checkType(expected, actual)) ()
    else Typer.error(s"Failed typecheck for $name. Expected: $expected, actual: $actual")
  }
}
object Typer {
  type TypeEnv = Env[RuntimeValue]
  val interpreter = new Interpreter

  def error(s: String) = throw TypeError(s)

  private def compileDef(d: Ast.Def, env: TypeEnv): IR.Def = {
    val funcType = env.get(d.name) match {
      case Some(f) => f.value.asInstanceOf[FuncType]
    }

    val argNames = d.args.map(_.name)
    val newEnv = argNames.zip(funcType.args).foldLeft(env.newScope) { case (curEnv, nextArg) =>
      curEnv.put(nextArg._1, Strict(nextArg._2))
    }
    val irBody = compileExpr(d.body, newEnv)
    TypeCheck.validateType(funcType.out, irBody.tpe, "out")

    IR.Def(
      d.name,
      d.args.map(_.name),
      irBody,
      funcType
    )
  }

  def toInterpreterStmt(e: Ast.TopLevelStmt, path: String): IA.Expr = e match {
    case d: Ast.Def => IA.Call(IA.Ident("Func"), (d.args.map(_.tpe) ::: d.outType :: Nil).map(toInterpreterExpr))
    case s: Ast.Struct =>
      IA.Call(
        IA.Ident("StructType"),
        IA.Literal(s.name) :: IA.Literal(path) :: s.args.flatMap(arg =>
          List(IA.Literal(arg.name), toInterpreterExpr(arg.tpe))
        )
      )
    case n: Ast.Namespace =>
      val newPath = if (path.isEmpty) n.name else s"$path.${n.name}"
      val stmts = toInterpreterStmts(n.children, newPath)
      IA.Block(
        stmts ::: IA.Call(
          IA.Ident("NamespaceType"),
          n.children.flatMap(c => IA.Literal(c.name) :: IA.Ident(c.name) :: Nil)
        ) :: Nil
      )

  }

  private def compileMemberRef(m: Ast.MemberRef, env: TypeEnv): IR.MemberRef = {
    val compiledExpr = compileExpr(m.expr, env)
    compiledExpr.tpe match {
      case s: StructType =>
        val resType = s.args.find(_._1 == m.memberName).map(_._2).getOrElse(error(s"Member ${m.memberName} not found"))
        IR.MemberRef(compiledExpr, m.memberName, resType)
      case n: NamespaceType =>
        val resType = n.children(m.memberName)
        IR.MemberRef(compiledExpr, m.memberName, resType)
      case other => error(s"Cannot get member of ${other}")
    }
  }

  private def compileCall(c: Ast.Call, env: TypeEnv): IR.Expr = {
    val compiledFunc = compileExpr(c.func, env)

    compiledFunc.tpe match {
      case f: FuncType =>
        val compiledArgs = c.args.map(a => compileExpr(a, env))
        f.args.zip(compiledArgs).foreach { case (expectedType, arg) =>
          TypeCheck.validateType(expectedType, arg.tpe, "")
        }
        IR.Call(compiledFunc, compiledArgs, f.out)
      case s: StructType =>
        val compiledArgs = c.args.map(a => compileExpr(a, env))
        s.args.zip(compiledArgs).foreach { case (expectedArg, arg) =>
          TypeCheck.validateType(expectedArg._2, arg.tpe, expectedArg._1)
        }
        IR.New(compiledArgs, s)
      case _ => error(s"Cannot call an object of type ${compiledFunc.tpe}")
    }

  }

  private def compileExpr(expr: Ast.Expr, env: TypeEnv): IR.Expr = expr match {
    case Ast.Literal(_, value) =>
      value match {
        case i: Int     => IR.IntLiteral(i)
        case b: Boolean => IR.BoolLiteral(b)
        case s: String  => IR.StrLiteral(s)
      }
    case Ast.Ident(_, name) =>
      val value = env.get(name).getOrElse(error(s"$name not found")).value match {
        case t: Type => t
      }
      IR.Ident(name, value)
    case Ast.If(_, expr, success, fail) =>
      val resolvedExpr = compileExpr(expr, env)
      val compiledSuccess = compileExpr(success, env)
      val compiledFail = compileExpr(fail, env)
      IR.If(resolvedExpr, compiledSuccess, compiledFail)
    case c: Ast.Call      => compileCall(c, env)
    case m: Ast.MemberRef => compileMemberRef(m, env)
    case b: Ast.Block     => IR.Block(compileAll(b.stmts, env.newScope)._1)
  }

  private def compileStmt(stmt: Ast.Stmt, env: TypeEnv): (IR.Stmt, TypeEnv) = stmt match {
    case expr: Ast.Expr => (compileExpr(expr, env), env)
    case Ast.Val(name, expr) =>
      val compiledExpr = compileExpr(expr, env)
      (IR.Let(name, compiledExpr), env.put(name, Strict(compiledExpr.tpe)))
  }

  private def compileAll(stmts: List[Ast.Stmt], env: TypeEnv): (List[IR.Stmt], TypeEnv) = {
    stmts.foldLeft((Nil: List[IR.Stmt], env)) { case ((curStmts, curEnv), nextStmt) =>
      val compiled = compileStmt(nextStmt, curEnv)
      (curStmts :+ compiled._1, compiled._2)
    }
  }

  def toInterpreterExpr(e: Ast.Expr): IA.Expr = e match {
    case Ast.Ident(_, name) => IA.Ident(name)
    case c: Ast.Call        => IA.Call(toInterpreterExpr(c.func), c.args.map(toInterpreterExpr))
    case m: Ast.MemberRef   => IA.Call(IA.Ident("getMember"), List(toInterpreterExpr(m.expr), IA.Literal(m.memberName)))
    case other              => error(s"$other unsupported in type expressions")
  }

  def toInterpreterStmts(stmts: List[Ast.TopLevelStmt], path: String): List[IA.Stmt] = {
    val namestmts = stmts.map(s => IA.Let(s.name, IA.Literal(null), isLazy = false))
    val topLevelStmts = stmts.map(s => IA.Set(s.name, toInterpreterStmt(s, path), isLazy = true))
    namestmts ::: topLevelStmts
  }

  private val intrinsics: Map[String, Any] = Map(
    "+" -> (FuncType(List(IntType, IntType), IntType)),
    "-" -> (FuncType(List(IntType, IntType), IntType)),
    "*" -> (FuncType(List(IntType, IntType), IntType)),
    "==" -> (FuncType(List(AnyType, AnyType), BoolType)),
    "Unit" -> (UnitType),
    "Any" -> (AnyType),
    "Int" -> (IntType),
    "String" -> (StrType),
    "Func" -> ((args: List[Any]) => {
      val tpes = args.collect { case t: Type => t }
      FuncType(tpes.take(tpes.length - 1), tpes.last)
    }),
    "StructType" -> RuntimeValue.Func({ case (name: String) :: (namespace: String) :: args =>
      val obj = args
        .grouped(2)
        .map { case (key: String) :: (tpe: Type) :: Nil =>
          key -> tpe
        }
        .toList
      StructType(name, namespace, obj)
    }),
    "NamespaceType" -> { (args: List[Any]) =>
      val map = args
        .grouped(2)
        .map { case (key: String) :: (value: Type) :: Nil =>
          key -> value
        }
        .toMap

      (NamespaceType(map))
    },
    "getMember" -> RuntimeValue.Func { case (n: NamespaceType) :: (key: String) :: Nil =>
      n.children(key)
    }
  )

  def compileTopLevel(stmts: List[Ast.TopLevelStmt], env: TypeEnv): List[IR.TopLevelStmt] = {
    stmts.map {
      case s: Ast.Struct =>
        val tpe = env.get(s.name) match {
          case Some(s) => s.value.asInstanceOf[StructType]
        }
        IR.Struct(tpe)
      case d: Ast.Def => compileDef(d, env)
      case n: Ast.Namespace =>
        val namespaceTpe = env.get(n.name) match {
          case Some(nt) => nt.value.asInstanceOf[NamespaceType]
        }
        val nextEnv = namespaceTpe.children.foldLeft(env.newScope) { case (curEnv, (name, tpe)) =>
          curEnv.put(name, Strict(tpe))
        }
        IR.Namespace(n.name, compileTopLevel(n.children, nextEnv))
    }
  }

  private val typerEnv = Env.make[RuntimeValue](
    intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
    None
  )

  def compile(program: Ast.Program): IR.Program = {
    val topLevelStmts = toInterpreterStmts(program.topLevelStmts, "")
    val newEnv = interpreter.runAll(typerEnv.newScope, topLevelStmts)._1
    val irTopLevel = compileTopLevel(program.topLevelStmts, newEnv)

    val body = program.stmts match {
      case (b: Block) :: Nil => b
      case _                 => Block(0, program.stmts)
    }

    IR.Program(irTopLevel, compileExpr(body, newEnv))
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
