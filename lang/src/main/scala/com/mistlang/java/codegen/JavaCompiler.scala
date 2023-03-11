package com.mistlang.java.codegen

import com.mistlang.lang.IR
import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types._

object JavaCompiler {
  val unitInstance = JavaAst.Ident("Unit.unit")

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType     => "Integer"
      case StrType     => "String"
      case BoolType    => "Boolean"
      case UnitType    => "Unit"
      case AnyType     => "Object"
      case f: FuncType => compileFunctionType(f)
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
      d.args.map(a => JavaAst.Arg(a.name, compileType(a.tpe))),
      compileType(d.outType),
      body
    )
  }

  def compileExpr(expr: IR.Expr): JavaAst.Expr = expr match {
    case IR.Ident(name, _) =>
      val newName = name match {
        case "+" => "plusOp"
        case "-" => "minusOp"
        case "*" => "productOp"
        case "==" => "eqIntOp"
        case _ => name
      }
      JavaAst.Ident(newName)
    case IR.Call(expr, args, _)     => JavaAst.Call(compileExpr(expr), args.map(compileExpr))
    case IR.If(expr, success, fail) => JavaAst.If(compileExpr(expr), compileExpr(success), compileExpr(fail))
    case IR.IntLiteral(i)           => JavaAst.IntLiteral(i)
    case IR.StrLiteral(s)           => JavaAst.StrLiteral(s)
    case IR.BoolLiteral(b)          => JavaAst.BoolLiteral(b)
  }

  def compileStmt(stmt: IR.BodyStmt): JavaAst.Stmt = stmt match {
    case IR.Let(name, expr) => JavaAst.Let(name, compileType(expr.tpe), compileExpr(expr))
    case expr: IR.Expr      => compileExpr(expr)
  }
  def compile(p: IR.Program): JavaAst.Program = {
    val allDefs = p.defs ::: IR.Def("run", Nil, p.body.lastOption.map(_.tpe).getOrElse(UnitType), p.body) :: Nil
    JavaAst.Program(allDefs.map(compileDef))
  }
}
