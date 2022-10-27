package com.mistlang.java.codegen

import com.mistlang.lang.RuntimeValue.IntVal
import com.mistlang.lang.Type.{BasicFuncType, FuncType, TupleType, UnitType}
import com.mistlang.lang.TypedAst._
import com.mistlang.lang.{TaggedType, Type, TypedAst}

object CodeGenerator {

  private def compileType(tpe: TaggedType): String = {
    tpe.t match {
      case Type.IntType   => "Integer"
      case Type.StrType   => "String"
      case Type.BoolType  => "Boolean"
      case Type.UnitType  => "void"
      case Type.AnyType   => "Object"
      case TupleType(arr) => s"Tuple${arr.length}<${arr.map(compileType).mkString(", ")}>"
      case f: FuncType =>
        f match {
          case b: BasicFuncType      => compileFunctionType(b)
          case _: Type.TypelevelFunc => ???
        }
    }
  }

  private def isUnit(tpe: TaggedType): Boolean = tpe.t == UnitType
  private def getType[T <: Type](tpe: TaggedType): T = tpe.t.asInstanceOf[T]

  private def compileFunctionType(func: BasicFuncType): String = {
    val argTypes = func.args.map(a => compileType(a.tpe))
    val outType = compileType(func.outType)

    val genericParams = if (isUnit(func.outType)) argTypes else argTypes :+ outType
    val classPrefix = if (isUnit(func.outType)) "VFunction" else "Function"
    s"${classPrefix}${func.args.length}<${genericParams.mkString(", ")}>"
  }

  private val binaryOperators = List("==", "+", "-", "*")

  private def compileCall(c: Call): String = {
    c.func match {
      case i: Ident if binaryOperators.contains(i.name) =>
        s"(${compile(c.args.head)} ${i.name} ${compile(c.args(1))})"
      case i: Ident if i.name == "mkTuple" => compileTuple(c)
      case i: Ident if i.name == "at" =>
        s"${compile(c.args.head)}._${c.args(1).tpe.tags("value").asInstanceOf[IntVal].value}()"
      case _ =>
        val funcType = getType[BasicFuncType](c.func.tpe)
        val apply = if (funcType.isLambda) ".apply" else ""
        s"""${compile(c.func)}$apply(${c.args.map(compile).mkString(", ")})"""
    }
  }

  private def compileDefs(stmts: List[TypedAst]): String =
    stmts.collect { case d: Def => compile(d) }.mkString("\n")

  private def compileNonDefs(stmts: List[TypedAst]): (String, TaggedType) = {
    val nonDefStmts = stmts.filter {
      case _: Def => false
      case _      => true
    }
    val nonDefs = nonDefStmts.map(compile).map(_ + ";")
    val addReturn = !isUnit(nonDefStmts.last.tpe)

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
    val funcName = if (isUnit(b.tpe)) "VFunction0" else s"Function0<$tpeName>"
    s"""new $funcName() {
       |  ${compileDefs(b.stmts)}
       |
       |  public $tpeName apply() {
       |    ${compileNonDefs(b.stmts)._1}
       |  }
       |}.apply()""".stripMargin
  }
  private def compileFunc(name: String, expr: Expr): String = {
    val (outType, args, body) = expr match {
      case func: Lambda =>
        val funcType = getType[BasicFuncType](func.tpe)
        val compiledArgs = funcType.args.map(a => s"${compileType(a.tpe)} ${a.name}").mkString(", ")
        val compiledBody = func.body match {
          case b: Block if !(b.stmts.exists {
                case _: Def => true
                case _      => false
              }) =>
            compileNonDefs(b.stmts)._1
          case other => "return " + compile(other) + ";"
        }
        (funcType.outType, compiledArgs, compiledBody)
      case expr: Expr =>
        val outType = expr.tpe
        val args = ""
        val body =
          s"""{
             |  return ${compile(expr)};
             |}
             |""".stripMargin
        (outType, args, body)
    }
    s"""public ${compileType(outType)} ${name}($args) {
       |  $body
       |}""".stripMargin
  }

  private def compileIf(i: If): String =
    s"""((${compile(i.expr)}) ? ${compile(i.success)} : ${compile(i.fail)})"""

  private def compileTuple(c: Call): String = {
    val tupleType = getType[TupleType](c.tpe)
    val genericTypes = tupleType.arr.map(compileType).mkString(", ")
    s"new Tuple${c.args.length}<$genericTypes>(${c.args.map(compile).mkString(", ")})"
  }

  private def compileLambda(l: Lambda): String = {
    val funcType = getType[BasicFuncType](l.tpe)
    s"""(new ${compileFunctionType(funcType)} () {
       |  ${compileFunc("apply", l)}
       |})""".stripMargin
  }

  private def compileIdent(i: Ident): String = {
    if (i.isLazy) i.name + "()"
    else i.name
  }

  def compile(ast: TypedAst): String = ast match {
    case expr: Expr =>
      expr match {
        case Literal(value, _) =>
          value match {
            case _: String => "\"" + value + "\""
            case _         => value.toString
          }
        case i: Ident     => compileIdent(i)
        case b: Block     => compileBlock(b)
        case c: Call      => compileCall(c)
        case l: Lambda    => compileLambda(l)
        case i: If        => compileIf(i)
        case l: Lazy      => compile(l.expr)
        case s: Synthetic => ""
      }
    case v: Val => s"var ${v.name} = ${compile(v.expr)}"
    case d: Def => compileFunc(d.name, d.expr)

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
