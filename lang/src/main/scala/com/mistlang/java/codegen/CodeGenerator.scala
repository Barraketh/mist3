package com.mistlang.java.codegen

import com.mistlang.java.codegen.JavaAst._

object CodeGenerator {

  private def compileCall(c: Call): String =
    s"""${compileExpr(c.expr)}.apply(${c.args.map(compileExpr).mkString(", ")})"""

  private def compileFunctionBody(stmts: List[Stmt]): String =
    stmts.map(compileStmt).mkString("\n")

  private def compileIf(i: If): String =
    s"""((${compileExpr(i.expr)}) ? ${compileExpr(i.success)} : ${compileExpr(i.fail)})"""

  private def compileFunc(func: Def): String = {
    val body = compileFunctionBody(func.body)
    val compiledArgs = func.args.map(a => s"${a.tpe} ${a.name}").mkString(", ")

    s"""
       |public static final ${func.funcType} ${func.name} = new ${func.funcType}() {
       |  private final ${func.funcType} ${func.name} = this;
       |
       |  public ${func.outType} apply($compiledArgs) {
       |    $body
       |  }
       |};
       |""".stripMargin
  }

  def compileExpr(expr: Expr): String = {
    expr match {
      case i: IntLiteral  => i.i.toString
      case b: BoolLiteral => b.b.toString
      case s: StrLiteral  => "\"" + s.s + "\""
      case i: Ident       => i.name
      case c: Call        => compileCall(c)
      case i: If          => compileIf(i)
    }
  }

  def compileStmt(ast: Stmt): String = ast match {
    case expr: Expr => compileExpr(expr) + ";"
    case l: Let =>
      val compiledExpr = compileExpr(l.expr)
      s"final ${l.tpe} ${l.name} = $compiledExpr;"
    case r: Return => s"return ${compileExpr(r.expr)};"
  }

  def compile(program: Program, pkg: List[String], className: String): String = {

    val pkgStmt = if (pkg.nonEmpty) s"package ${pkg.mkString(".")};\n" else ""
    s"""$pkgStmt
       |import com.mistlang.java.stdlib.*;
       |import com.mistlang.java.stdlib.Functions.*;
       |
       |import static com.mistlang.java.stdlib.StdFunctions.*;
       |
       |public class $className {
       |  ${program.functions.map(compileFunc).mkString("\n")}
       |}""".stripMargin
  }

}
