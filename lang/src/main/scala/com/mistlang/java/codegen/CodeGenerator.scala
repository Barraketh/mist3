package com.mistlang.java.codegen

import com.mistlang.lang.IR._
import com.mistlang.lang.RuntimeValue.Dict
import com.mistlang.lang.Types._
import com.mistlang.lang.{IR, RuntimeValue, Typer}

object CodeGenerator {

  private def compileType(tpe: RuntimeValue): String = {
    def typeMatches(expected: RuntimeValue): Boolean = Typer.checkType(expected, tpe)

    if (typeMatches(IntType)) "Integer"
    else if (typeMatches(StrType)) "String"
    else if (typeMatches(BoolType)) "Boolean"
    else if (typeMatches(UnitType)) "Unit"
    else if (typeMatches(FuncType)) compileFunctionType(tpe.getDict)
    else if (typeMatches(AnyType)) "Object"
    else Typer.error(s"Unknown type ${tpe}")
  }

  private def compileFunctionType(func: Dict): String = {
    val argTypes = BasicFuncType.getArgs(func).map(a => compileType(a._2)) //func.args.map(a => compileType(a._2))
    val outType = compileType(func("out"))

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  private val binaryOperators = List("==", "+", "-", "*")

  private def compileCall(c: Call): String = {
    c.expr match {
      case i: Ident if binaryOperators.contains(i.name) =>
        s"(${compile(c.args.head)} ${i.name} ${compile(c.args(1))})"
      case i: Ident if i.name == "get" =>
        val fields = c.args.head.tpe("fields").getDict.fields.keys.toList.sorted
        val idx = fields.indexOf(c.args(1).tpe("value").getString)
        s"${compile(c.args.head)}._$idx()"
      case _ =>
        s"""${compile(c.expr)}.apply(${c.args.map(compile).mkString(", ")})"""
    }
  }

  private def compileAll(stmts: List[IR]): String = {
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
      val compiledArgs = BasicFuncType.getArgs(func.tpe).map(a => s"${compileType(a._2)} ${a._1}").mkString(", ")
      val compiledBody = compileAll(func.body)
      (func.tpe("out"), compiledArgs, compiledBody)
    }
    s"""public ${compileType(outType)} ${name}($args) {
       |  $body
       |}""".stripMargin
  }

  private def compileRecord(record: Record): String = {
    val argTypes = record.tpe("fields").getDict
    val args = record.rows.sortBy(_.key)
    val genericTypes = args.map(r => compileType(argTypes(r.key))).mkString(", ")
    s"new Tuple${args.length}<$genericTypes>(${args.map(a => compile(a.value)).mkString(", ")})"
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
        case i: Ident =>
          if (Typer.checkType(MutableType, i.tpe)) s"${i.name}.value"
          else i.name
        case c: Call   => compileCall(c)
        case l: Lambda => compileLambda(l)
        case i: If     => compileIf(i)
        case r: Record => compileRecord(r)
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
       |public class $className {
       |
       |  public ${compileType(asts.last.tpe)} run() {
       |
       |    ${compileAll(asts)}
       |  }
       |}""".stripMargin
  }

}
