package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.Value
import com.mistlang.interpreter.{Env, Interpreter, InterpreterAst, RuntimeValue}
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

  private def compileDef(d: Ast.Def, funcType: FuncType, env: TypeEnv): IR.Def = {

    val argNames = d.args.map(_.name)
    val newEnv = argNames.zip(funcType.args).foldLeft(env.newScope) { case (curEnv, nextArg) =>
      curEnv.put(nextArg._1, Value(nextArg._2))
    }
    val irBody = compileAll(d.body, newEnv)._1
    val outType = irBody.lastOption match {
      case Some(e: IR.Expr) => e.tpe
      case _                => UnitType
    }
    TypeCheck.validateType(funcType.out, outType, "out")

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
        case Value(t) => t
      }
    }
    val outType = interpreter.evalExpr(env, toInterpreterExpr(d.outType)) match {
      case Value(tpe) => tpe
    }
    FuncType(argTypes, outType)
  }

  private def getStructType(s: Ast.Struct, env: TypeEnv): StructType = {
    val args = s.args.map { arg =>
      arg.name -> (interpreter.evalExpr(env, toInterpreterExpr(arg.tpe)) match {
        case Value(t) => t
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
        case Value(t) => t
      }
      IR.Ident(name, value)
    case Ast.If(expr, success, fail) =>
      val resolvedExpr = compileExpr(expr, env)
      val compiledSuccess = compileExpr(success, env)
      val compiledFail = compileExpr(fail, env)
      IR.If(resolvedExpr, compiledSuccess, compiledFail)
    case c: Ast.Call      => compileCall(c, env)
    case m: Ast.MemberRef => compileMemberRef(m, env)
  }

  private def compileStmt(stmt: Ast.Stmt, env: TypeEnv): (IR.BodyStmt, TypeEnv) = stmt match {
    case expr: Ast.Expr => (compileExpr(expr, env), env)
    case Ast.Val(name, expr) =>
      val compiledExpr = compileExpr(expr, env)
      (IR.Let(name, compiledExpr), env.put(name, Value(compiledExpr.tpe)))
  }

  private def compileAll(stmts: List[Ast.Stmt], env: TypeEnv): (List[IR.BodyStmt], TypeEnv) = {
    stmts.foldLeft((Nil: List[IR.BodyStmt], env)) { case ((curStmts, curEnv), nextStmt) =>
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
    "+" -> Value(FuncType(List(IntType, IntType), IntType)),
    "-" -> Value(FuncType(List(IntType, IntType), IntType)),
    "*" -> Value(FuncType(List(IntType, IntType), IntType)),
    "==" -> Value(FuncType(List(AnyType, AnyType), BoolType)),
    "Unit" -> Value(UnitType),
    "Any" -> Value(AnyType),
    "Int" -> Value(IntType),
    "String" -> Value(StrType),
    "Func" -> RuntimeValue.Func[Type](args => {
      val tpes = args.collect { case RuntimeValue.Value(t) =>
        t
      }
      RuntimeValue.Value(FuncType(tpes.take(tpes.length - 1), tpes.last))
    })
  )

  private val typerEnv = Env.make(
    intrinsics.map { case (name, v) => name -> v },
    None
  )

  def compile(program: Ast.Program): IR.Program = {
    // TODO: implement lazyness so we can have forward sight?
    val envWithStructs = program.structs.foldLeft(typerEnv) { case (curEnv, struct) =>
      curEnv.put(struct.name, Value(getStructType(struct, curEnv)))
    }

    val funcTypes = program.defs.map { d =>
      d.name -> getFuncType(d, envWithStructs)
    }.toMap

    val newEnv = program.defs.foldLeft(envWithStructs) { case (curEnv, nextDef) =>
      curEnv.put(nextDef.name, Value(funcTypes(nextDef.name)))
    }

    val structs = program.structs
      .map(s =>
        newEnv.get(s.name) match {
          case Some(Value(s: StructType)) => s
        }
      )
      .map(s => IR.Struct(s))
    val defs = program.defs.map(d => compileDef(d, funcTypes(d.name), newEnv))

    IR.Program(structs, defs, compileAll(program.stmts, newEnv)._1)
  }
}

case class TypeError(msg: String) extends RuntimeException(msg)
