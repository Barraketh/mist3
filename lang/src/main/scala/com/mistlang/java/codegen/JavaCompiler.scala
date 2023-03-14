package com.mistlang.java.codegen

import com.mistlang.lang.Types._
import com.mistlang.lang.{IR, Type}

object JavaCompiler {
  val unitInstance = JavaAst.Ident("Unit.unit")

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType             => "Integer"
      case StrType             => "String"
      case BoolType            => "Boolean"
      case UnitType            => "Unit"
      case AnyType             => "Object"
      case StructType(name, _) => name
      case f: FuncType         => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: FuncType): String = {
    val argTypes = func.args.map(compileType)
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  def compileDef(d: IR.Def): JavaAst.Def = {
    val body: List[JavaAst.Stmt] = d.body.lastOption match {
      case Some(e: IR.Expr) =>
        d.body.take(d.body.length - 1).map(compileStmt) ::: JavaAst.Return(compileExpr(e)) :: Nil
      case _ => d.body.map(compileStmt) ::: JavaAst.Return(unitInstance) :: Nil
    }
    JavaAst.Def(
      compileType(d.tpe),
      d.name,
      d.args.zip(d.tpe.args).map { case (argName, argTpe) => JavaAst.Arg(argName, compileType(argTpe)) },
      compileType(d.tpe.out),
      body
    )
  }

  def compileStruct(s: IR.Struct): JavaAst.Struct = {
    JavaAst.Struct(s.tpe.name, s.tpe.args.map(arg => JavaAst.Arg(arg._1, compileType(arg._2))))
  }

  def compileExpr(expr: IR.Expr): JavaAst.Expr = expr match {
    case IR.Ident(name, _) =>
      val newName = name match {
        case "+"  => "plusOp"
        case "-"  => "minusOp"
        case "*"  => "productOp"
        case "==" => "eqIntOp"
        case _    => name
      }
      JavaAst.Ident(newName)
    case IR.Call(expr, args, _)            => JavaAst.Call(compileExpr(expr), args.map(compileExpr))
    case IR.If(expr, success, fail)        => JavaAst.If(compileExpr(expr), compileExpr(success), compileExpr(fail))
    case IR.IntLiteral(i)                  => JavaAst.IntLiteral(i)
    case IR.StrLiteral(s)                  => JavaAst.StrLiteral(s)
    case IR.BoolLiteral(b)                 => JavaAst.BoolLiteral(b)
    case IR.MemberRef(expr, memberName, _) => JavaAst.MemberRef(compileExpr(expr), memberName)
    case IR.New(args, tpe)                 => JavaAst.New(compileType(tpe), args.map(compileExpr))

  }

  def compileStmt(stmt: IR.BodyStmt): JavaAst.Stmt = stmt match {
    case IR.Let(name, expr) => JavaAst.Let(name, compileType(expr.tpe), compileExpr(expr))
    case expr: IR.Expr      => compileExpr(expr)
  }
  def compile(p: IR.Program): JavaAst.Program = {
    val structs = p.structs.map(compileStruct)
    val outType = p.body.lastOption match {
      case Some(e: IR.Expr) => e.tpe
      case _                => UnitType
    }
    val allDefs = p.defs ::: IR.Def("run", Nil, p.body, FuncType(Nil, outType)) :: Nil
    JavaAst.Program(structs, allDefs.map(compileDef))
  }
}
