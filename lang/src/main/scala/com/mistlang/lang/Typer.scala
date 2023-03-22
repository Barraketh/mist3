package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Strict, Value}
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
  type TypeEnv = Env[RuntimeValue[Any]]
  val interpreter = new Interpreter[Any]

  def error(s: String) = throw TypeError(s)

  private def compileDef(d: Ast.Def, env: TypeEnv): IR.Def = {
    val funcType = env.get(d.name) match {
      case Some(f: Value[FuncType]) => f.value
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

  def compileTopLevel(e: Ast.TopLevelStmt): IA.Expr[Any] = e match {
    case d: Ast.Def => IA.Call(IA.Ident("Func"), (d.args.map(_.tpe) ::: d.outType :: Nil).map(toInterpreterExpr))
    case s: Ast.Struct =>
      IA.Call(
        IA.Ident("Struct"),
        IA.Literal(s.name) :: s.args.flatMap(arg => List(IA.Literal(arg.name), toInterpreterExpr(arg.tpe)))
      )
  }

  private def compileMemberRef(m: Ast.MemberRef, env: TypeEnv): IR.MemberRef = {
    val compiledExpr = compileExpr(m.expr, env)
    compiledExpr.tpe match {
      case s: StructType =>
        val resType = s.args.find(_._1 == m.memberName).map(_._2).getOrElse(error(s"Member ${m.memberName} not found"))
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
    case Ast.Literal(value) =>
      value match {
        case i: Int     => IR.IntLiteral(i)
        case b: Boolean => IR.BoolLiteral(b)
        case s: String  => IR.StrLiteral(s)
      }
    case Ast.Ident(name) =>
      val value = env.get(name).getOrElse(error(s"$name not found")) match {
        case t: Value[Type] => t.value
      }
      IR.Ident(name, value)
    case Ast.If(expr, success, fail) =>
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

  def toInterpreterExpr(e: Ast.Expr): IA.Expr[Type] = e match {
    case Ast.Ident(name) => IA.Ident(name)
    case c: Ast.Call     => IA.Call(toInterpreterExpr(c.func), c.args.map(toInterpreterExpr))
    case _               => error("Only type refs currently supported in type expressions")
  }

  def compileTopLevel(stmts: List[Ast.TopLevelStmt]): List[IA.Stmt[Any]] = {
    val namestmts = stmts.map(s => IA.Let(s.name, IA.Literal(null)))
    val topLevelStmts = stmts.map(s => IA.Set(s.name, IA.Lazy(compileTopLevel(s))))
    namestmts ::: topLevelStmts
  }

  private val intrinsics: Map[String, RuntimeValue[Any]] = Map(
    "+" -> Strict(FuncType(List(IntType, IntType), IntType)),
    "-" -> Strict(FuncType(List(IntType, IntType), IntType)),
    "*" -> Strict(FuncType(List(IntType, IntType), IntType)),
    "==" -> Strict(FuncType(List(AnyType, AnyType), BoolType)),
    "Unit" -> Strict(UnitType),
    "Any" -> Strict(AnyType),
    "Int" -> Strict(IntType),
    "String" -> Strict(StrType),
    "Func" -> RuntimeValue.Func[Type](args => {
      val tpes = args.collect { case t: Value[Type] => t.value }
      RuntimeValue.Strict(FuncType(tpes.take(tpes.length - 1), tpes.last))
    }),
    "Struct" -> RuntimeValue.Func[Any] { case (name: Value[String]) :: args =>
      val obj = args
        .grouped(2)
        .map { case (key: Value[String]) :: (tpe: Value[Type]) :: Nil =>
          key.value -> tpe.value
        }
        .toList
      RuntimeValue.Strict(StructType(name.value, obj))
    }
  )

  private val typerEnv = Env.make[RuntimeValue[Any]](
    intrinsics.map { case (name, v) => name -> v },
    None
  )

  def compile(program: Ast.Program): IR.Program = {
    val topLevelStmts = compileTopLevel(program.topLevelStmts)
    val newEnv = interpreter.runAll(typerEnv, topLevelStmts)._1

    val structs = program.topLevelStmts
      .collect { case s: Ast.Struct =>
        newEnv.get(s.name) match {
          case Some(s: Value[StructType]) => s.value
        }
      }
      .map(s => IR.Struct(s))

    val defs = program.topLevelStmts.collect { case d: Ast.Def =>
      compileDef(d, newEnv)
    }

    val body = program.stmts match {
      case (b: Block) :: Nil => b
      case _                 => Block(program.stmts)
    }

    IR.Program(structs, defs, compileExpr(body, newEnv))
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
