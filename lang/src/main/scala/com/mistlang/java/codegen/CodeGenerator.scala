package com.mistlang.java.codegen

import com.mistlang.java.codegen.JavaAst._

object CodeGenerator {

  private def compileCall(c: Call): String =
    s"""${compileExpr(c.expr)}.apply(${c.args.map(compileExpr).mkString(", ")})"""

  private def compileNew(n: New): String =
    s"""(new ${n.tpe}(${n.args.map(compileExpr).mkString(", ")}))"""

  private def compileStmts(stmts: List[Stmt]): String =
    stmts.map(compileStmt).mkString("\n")

  private def compileIf(i: IfExpr): String =
    s"""((${compileExpr(i.expr)}) ? ${compileExpr(i.success)} : ${compileExpr(i.fail)})"""

  private def compileFunc(func: Def): String = {
    val body = compileStmts(func.body)
    val compiledArgs = func.args.map(a => s"${a.tpe} ${a.name}").mkString(", ")

    s"""
       |public static final ${func.funcType} ${func.name} = new ${func.funcType}() {
       |  public ${func.outType} apply($compiledArgs) {
       |    $body
       |  }
       |};
       |""".stripMargin
  }

  private def compileStruct(s: Struct): String = {
    val compiledArgs = s.args.map(a => s"${a.tpe} ${a.name}").mkString(", ")
    s"""
       |public record ${s.name}($compiledArgs){}
       |""".stripMargin
  }

  def compileExpr(expr: Expr): String = {
    expr match {
      case i: IntLiteral  => i.i.toString
      case b: BoolLiteral => b.b.toString
      case s: StrLiteral  => "\"" + s.s + "\""
      case i: Ident       => i.name
      case c: Call        => compileCall(c)
      case i: IfExpr      => compileIf(i)
      case m: MemberRef   => compileExpr(m.expr) + s".${m.memberName}"
      case n: New         => compileNew(n)
    }
  }

  def compileStmt(ast: Stmt): String = ast match {
    case expr: Expr => compileExpr(expr) + ";"
    case decl: Decl => s"final ${decl.tpe} ${decl.name};"
    case ifStmt: IfStmt =>
      s"""if (${compileExpr(ifStmt.cond)}) {
         |  ${compileStmts(ifStmt.success)}
         |} else {
         |  ${compileStmts(ifStmt.fail)}
         |}
         |""".stripMargin
    case s: Set => s"${s.name} = ${compileExpr(s.expr)};"
    case b: Block =>
      s"""{
         |  ${compileStmts(b.stmts)}
         |}
         |""".stripMargin
    case l: Let =>
      val compiledExpr = compileExpr(l.expr)
      s"final ${l.tpe} ${l.name} = $compiledExpr;"
    case r: Return => s"return ${compileExpr(r.expr)};"
  }

  private def compileNamespace(n: Namespace): String = {
    s"""static class ${n.name} {
       |  ${n.stmts.map(compileTopLevel).mkString("\n")}
       |}
       |""".stripMargin
  }

  private def compileTopLevel(stmt: JavaAst.TopLevelStmt): String = stmt match {
    case s: JavaAst.Struct => compileStruct(s)
    case d: JavaAst.Def    => compileFunc(d)
    case n: Namespace      => compileNamespace(n)
  }

  def compile(program: Program, pkg: List[String], className: String): String = {
    val classBody = program.stmts.map(compileTopLevel).mkString("\n")

    val pkgStmt = if (pkg.nonEmpty) s"package ${pkg.mkString(".")};\n" else ""
    s"""$pkgStmt
       |import com.mistlang.java.stdlib.*;
       |import com.mistlang.java.stdlib.Functions.*;
       |
       |import static com.mistlang.java.stdlib.StdFunctions.*;
       |
       |public class $className {
       |  $classBody
       |}""".stripMargin
  }

}
