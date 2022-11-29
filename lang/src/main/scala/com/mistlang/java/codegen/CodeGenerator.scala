package com.mistlang.java.codegen

import com.mistlang.lang.IR
import com.mistlang.lang.IR._
import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Type._

object CodeGenerator {

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType  => "Integer"
      case StrType  => "String"
      case BoolType => "Boolean"
      case UnitType => "Unit"
      case AnyType  => "Object"
      case f: FuncType =>
        f match {
          case b: BasicFuncType      => compileFunctionType(b)
          case _: Type.TypelevelFunc => ???
        }
    }
  }

  private def compileFunctionType(func: BasicFuncType): String = {
    val argTypes = func.args.map(a => compileType(a._2))
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${func.args.length}<${genericParams.mkString(", ")}>"
  }

  private val binaryOperators = List("==", "+", "-", "*")

  private def compileCall(c: Call): String = {
    c.expr match {
      case i: Ident if binaryOperators.contains(i.name) =>
        s"(${compile(c.args.head)} ${i.name} ${compile(c.args(1))})"
      case _ =>
        val funcType = c.expr.tpe.asInstanceOf[BasicFuncType]
        val apply = if (funcType.isLambda) ".apply" else ""
        s"""${compile(c.expr)}$apply(${c.args.map(compile).mkString(", ")})"""
    }
  }

  private def compileAll(stmts: List[BodyStmt]): String = {
    val compiled = stmts.map(compile).map(_ + ";")
    val withReturn = stmts.lastOption match {
      case Some(ir) if ir.tpe != UnitType =>
        (compiled.reverse match {
          case head :: tail => s"return $head" :: tail
          case Nil          => throw new RuntimeException("WTF")
        }).reverse
      case _ => compiled :+ s"return Unit.unit;"
    }

    withReturn.mkString("\n")
  }

  private def compileIf(i: If): String =
    s"""((${compile(i.expr)}) ? ${compile(i.success)} : ${compile(i.fail)})"""

  private def compileFunc(name: String, func: Lambda): String = {
    val (outType, args, body) = {
      val funcType = func.tpe
      val compiledArgs = funcType.args.map(a => s"${compileType(a._2)} ${a._1}").mkString(", ")
      val compiledBody = compileAll(func.body)
      (funcType.out, compiledArgs, compiledBody)
    }
    s"""public ${compileType(outType)} ${name}($args) {
       |  $body
       |}""".stripMargin
  }

  private def compileLambda(l: Lambda): String = {
    s"""(new ${compileFunctionType(l.tpe)} () {
       |  ${compileFunc("apply", l)}
       |})""".stripMargin
  }

  def compile(ast: IR): String = ast match {
    case expr: Expr =>
      expr match {
        case i: IntLiteral  => i.i.toString
        case b: BoolLiteral => b.b.toString
        case s: StrLiteral  => "\"" + s + "\""
        case i: Ident       => i.name
        case c: Call        => compileCall(c)
        case l: Lambda      => compileLambda(l)
        case i: If          => compileIf(i)
      }
    case l: Let => s"final var ${l.name} = ${compile(l.expr)}"
    case d: Def => compileFunc(d.name, d.l)
  }

  def compile(asts: List[IR], pkg: List[String], className: String): String = {
    val defs = asts.collect { case d: Def => d }
    val body = asts.collect { case b: BodyStmt => b }

    val compiledDefs = defs.map(compile).mkString("\n")

    val pkgStmt = if (pkg.nonEmpty) s"package ${pkg.mkString(".")};\n" else ""
    s"""$pkgStmt
       |import com.mistlang.java.stdlib.*;
       |import com.mistlang.java.stdlib.Functions.*;
       |
       |public class $className {
       |  ${compiledDefs}
       |
       |  public ${compileType(asts.last.tpe)} run() {
       |    ${compileAll(body)}
       |  }
       |}""".stripMargin
  }

}
