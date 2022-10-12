package com.mistlang.java.codegen

import com.mistlang.lang.Type.{FuncType, TupleType, UnitType}
import com.mistlang.lang.TypedAst._
import com.mistlang.lang.{Type, TypedAst}

object CodeGenerator {

  private def compileType(tpe: Type): String = {
    tpe match {
      case Type.IntType   => "Integer"
      case Type.StrType   => "String"
      case Type.BoolType  => "Boolean"
      case Type.UnitType  => "void"
      case Type.AnyType   => "Object"
      case TupleType(arr) => s"Tuple${arr.length}<${arr.map(compileType).mkString(", ")}>"
      case f: FuncType    => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: FuncType): String = {
    val argTypes = func.args.map(a => compileType(a.tpe))
    val outType = compileType(func.outType)

    val genericParams = if (func.outType == UnitType) argTypes else argTypes :+ outType
    val classPrefix = if (func.outType == UnitType) "VFunction" else "Function"
    s"${classPrefix}${func.args.length}<${genericParams.mkString(", ")}>"
  }

  private val binaryOperators = List("==", "+", "-", "*")

  private def compileCall(c: Call): String = {
    c.func match {
      case i: Ident if binaryOperators.contains(i.name) =>
        s"(${compile(c.args.head)} ${i.name} ${compile(c.args(1))})"
      case i: Ident if i.name == "at" =>
        s"${compile(c.args.head)}._${c.args(1).asInstanceOf[Literal].value}"
      case _ =>
        val apply = if (c.func.tpe.asInstanceOf[FuncType].isLambda) ".apply" else ""
        s"""${compile(c.func)}$apply(${c.args.map(compile).mkString(", ")})"""
    }
  }

  private def compileDefs(stmts: List[TypedAst]): String =
    stmts.collect { case d: Def => compile(d) }.mkString("\n")

  private def compileNonDefs(stmts: List[TypedAst]): (String, Type) = {
    val nonDefStmts = stmts.filter {
      case _: Def => false
      case _      => true
    }
    val nonDefs = nonDefStmts.map(compile).map(_ + ";")
    val addReturn = nonDefStmts.last.tpe != UnitType

    val withReturn = if (addReturn) {
      (nonDefs.reverse match {
        case head :: tail => s"return $head" :: tail
        case Nil          => throw new RuntimeException("WTF")
      }).reverse
    } else nonDefs

    (withReturn.mkString("\n"), nonDefStmts.last.tpe)
  }

  private def compileBlock(b: Block): String = {
    val tpeName = compileType(b.tpe)
    val funcName = b.tpe match {
      case UnitType => "VFunction0"
      case _        => s"Function0<$tpeName>"
    }
    s"""new $funcName() {
       |  ${compileDefs(b.stmts)}
       |
       |  public $tpeName apply() {
       |    ${compileNonDefs(b.stmts)._1}
       |  }
       |}.apply()""".stripMargin
  }
  private def compileFunc(name: String, func: Lambda): String = {
    val compiledArgs = func.tpe.args.map(a => s"${compileType(a.tpe)} ${a.name}").mkString(", ")
    val compiledBody = func.body match {
      case b: Block if !(b.stmts.exists {
            case _: Def => true
            case _      => false
          }) =>
        compileNonDefs(b.stmts)._1
      case other => "return " + compile(other) + ";"
    }
    s"""public ${compileType(func.tpe.outType)} ${name}($compiledArgs) {
       |  $compiledBody
       |}""".stripMargin
  }

  private def compileIf(i: If): String =
    s"""((${compile(i.expr)}) ? ${compile(i.success)} : ${compile(i.fail)})"""

  private def compileTuple(t: Tuple): String =
    s"new Tuple${t.exprs.length}(${t.exprs.map(compile).mkString(", ")})"

  private def compileLambda(l: Lambda): String = {
    s"""(new ${compileFunctionType(l.tpe)} () {
       |  ${compileFunc("apply", l)}
       |})""".stripMargin
  }

  def compile(ast: TypedAst): String = ast match {
    case expr: Expr =>
      expr match {
        case Literal(value, _) =>
          value match {
            case _: String => "\"" + value + "\""
            case _         => value.toString
          }
        case Ident(name, _) => name
        case t: Tuple       => compileTuple(t)
        case b: Block       => compileBlock(b)
        case c: Call        => compileCall(c)
        case l: Lambda      => compileLambda(l)
        case i: If          => compileIf(i)
        case s: Synthetic   => ""
      }
    case v: Val => s"var ${v.name} = ${compile(v.expr)}"
    case d: Def => compileFunc(d.name, d.func)

  }

  def compile(asts: List[TypedAst], pkg: List[String], className: String): String = {
    val (nonDefs, returnType) = compileNonDefs(asts)
    val pkgStmt = if (pkg.nonEmpty) s"package ${pkg.mkString(".")};\n" else ""
    s"""$pkgStmt
       |import com.mistlang.java.stdlib.*;
       |import com.mistlang.java.stdlib.Tuples.*;
       |import com.mistlang.java.stdlib.Functions.*;
       |
       |public class $className {
       |  ${compileDefs(asts)}
       |
       |  public ${compileType(returnType)} run() {
       |    $nonDefs
       |  }
       |}""".stripMargin
  }

}