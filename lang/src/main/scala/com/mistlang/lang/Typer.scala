package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict, Value}
import com.mistlang.interpreter.{Env, Interpreter, InterpreterAst, RuntimeValue}
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
  type TypeEnv = Env[RuntimeValue[Type]]
  val interpreter = new Interpreter[Type]

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

  private def getFuncType(d: Ast.Def, env: TypeEnv): FuncType = {
    val argTypes = d.args.map { arg =>
      interpreter.evalExpr(env, toInterpreterExpr(arg.tpe)) match {
        case t: Value[Type] => t.value
      }
    }
    val outType = interpreter.evalExpr(env, toInterpreterExpr(d.outType)) match {
      case tpe: Value[Type] => tpe.value
    }
    FuncType(argTypes, outType)
  }

  private def getStructType(s: Ast.Struct, env: TypeEnv): StructType = {
    val args = s.args.map { arg =>
      arg.name -> (interpreter.evalExpr(env, toInterpreterExpr(arg.tpe)) match {
        case t: Value[Type] => t.value
      })
    }
    StructType(s.name, args)
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

  def toInterpreterExpr(e: Ast.Expr): InterpreterAst.Expr[Type] = e match {
    case Ast.Ident(name) => InterpreterAst.Ident(name)
    case c: Ast.Call     => InterpreterAst.Call(toInterpreterExpr(c.func), c.args.map(toInterpreterExpr))
    case _               => error("Only type refs currently supported in type expressions")
  }

  private val intrinsics: Map[String, RuntimeValue[Type]] = Map(
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
    })
  )

  private val typerEnv = Env.make(
    intrinsics.map { case (name, v) => name -> v },
    None
  )

  def compile(program: Ast.Program): IR.Program = {
    val newEnv = program.topLevelStmts.foldLeft(typerEnv) { case (curEnv, stmt) =>
      curEnv.put(stmt.name, Strict(null))
    }

    program.topLevelStmts.foreach { stmt =>
      val res = stmt match {
        case d: Ast.Def    => () => getFuncType(d, newEnv)
        case s: Ast.Struct => () => getStructType(s, newEnv)
      }
      newEnv.set(stmt.name, Lazy(res))
    }

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
