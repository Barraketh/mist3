package com.mistlang.java.codegen

import com.mistlang.lang.IR._
import com.mistlang.lang.RuntimeValue.Type
import com.mistlang.lang.RuntimeValue.Types._
import com.mistlang.lang.{IR, TypeCheck}

object CodeGenerator {

  private def compileType(tpe: Type): String = {
    tpe.tpe match {
      case IntType => "Integer"
      case StrType => "String"
      case BoolType => "Boolean"
      case UnitType => "Unit"
      case AnyType => "Object"
      case f: BasicFuncType => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: BasicFuncType): String = {
    val argTypes = func.args.map(compileType) //func.args.map(a => compileType(a._2))
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  private val binaryOperators = List("==", "+", "-", "*")

  private def compileCall(c: Call): String = {
    c.expr match {
      case i: Ident if binaryOperators.contains(i.name) =>
        s"(${compile(c.args.head)} ${i.name} ${compile(c.args(1))})"
      case _ =>
        s"""${compile(c.expr)}.apply(${c.args.map(compile).mkString(", ")})"""
    }
  }

  private def compileAll(stmts: List[IR]): String = {
    val compiled = stmts.map(compile).map(_ + ";")
    val withReturn = stmts.lastOption match {
      case Some(ir) if ir.tpe != UnitTypeInstance =>
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

  private def compileFunc(func: Lambda): String = {
    val (outType, args, body) = {
      val funcType = func.tpe.getFuncType
      val compiledTypes = funcType.args.map(compileType)
      val compiledArgs = func.argNames.zip(compiledTypes).map(a => s"${a._2} ${a._1}").mkString(", ")
      val compiledBody = compileAll(func.body)
      (funcType.out, compiledArgs, compiledBody)
    }
    s"""public ${compileType(outType)} apply($args) {
       |  $body
       |}""".stripMargin
  }

  private def compileLambda(l: Lambda): String = {
    s"""(new ${compileFunctionType(l.tpe.getFuncType)} () {
       |  ${compileFunc(l)}
       |})""".stripMargin
  }

  def compile(ast: IR): String = ast match {
    case expr: Expr =>
      expr match {
        case i: IntLiteral  => i.i.toString
        case b: BoolLiteral => b.b.toString
        case s: StrLiteral  => "\"" + s.s + "\""
        case i: Ident =>
          if (TypeCheck.isMutable(i.tpe)) s"${i.name}.value"
          else i.name
        case c: Call   => compileCall(c)
        case l: Lambda => compileLambda(l)
        case i: If     => compileIf(i)
        case n: Null   => s"(${compileType(n.tpe)})null"
      }
    case l: Let =>
      val compiledExpr = compile(l.expr)
      val rhv =
        if (l.isMutable) s"new MutableRef<${compileType(l.expr.tpe)}>($compiledExpr)"
        else compiledExpr
      s"final var ${l.name} = $rhv"
    case s: IR.Set =>
      s"${s.name}.set(${compile(s.expr)})"
  }

  def compile(asts: List[IR], pkg: List[String], className: String): String = {

    val pkgStmt = if (pkg.nonEmpty) s"package ${pkg.mkString(".")};\n" else ""
    s"""$pkgStmt
       |import com.mistlang.java.stdlib.*;
       |import com.mistlang.java.stdlib.Tuples.*;
       |import com.mistlang.java.stdlib.Functions.*;
       |
       |import static com.mistlang.java.stdlib.StdFunctions.*;
       |
       |public class $className {
       |
       |  public ${compileType(asts.last.tpe)} run() {
       |
       |    ${compileAll(asts)}
       |  }
       |}""".stripMargin
  }

}
