package com.mistlang.java.codegen

import com.mistlang.lang
import com.mistlang.lang.ComptimeValue.CachingFunc
import com.mistlang.lang.TypeInterpreter.TypeCache
import com.mistlang.lang.Types._
import com.mistlang.lang.{Ast, Type, TypeInterpreter, TypedValue}

class JavaCompiler {
  private val unitInstance = JavaAst.Ident("Unit.unit")

  private val structNameCache = scala.collection.mutable.Map[StructType, String]()

  private var curId = 0
  private def nextId(): Int = {
    curId = curId + 1
    curId
  }

  private def ce(expr: JavaAst.Expr) = CompiledExpr(Nil, expr)

  private def compileExprs(
      exprs: List[Ast.Expr],
      types: lang.TypeInterpreter.TypeCache
  ): List[CompiledExpr] = {
    exprs.map(e => compileExpr(e, types))
  }

  private def compileStmts(
      stmts: List[Ast.FnBodyStmt],
      types: TypeInterpreter.TypeCache
  ): List[JavaAst.Stmt] = {
    stmts.flatMap(stmt => compileStmt(stmt, types))
  }

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType               => "Integer"
      case StrType               => "String"
      case BoolType              => "Boolean"
      case UnitType              => "Unit"
      case AnyType               => "Object"
      case ArrayType(underlying) => compileType(underlying) + "[]"
      case s: StructType         => structNameCache(s)
      case f: FuncType           => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: FuncType): String = {
    val argTypes = func.args.map(compileType)
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  private def compileLambda(l: Ast.Lambda, types: TypeInterpreter.TypeCache): JavaAst.Lambda = {
    val compiledStmts = l.body match {
      case b: Ast.Block => compileStmts(b.stmts, types)
      case other        => compileExpr(other, types).asStmts
    }
    val body = compiledStmts.lastOption match {
      case Some(expr: JavaAst.Expr) => compiledStmts.take(compiledStmts.length - 1) ::: JavaAst.Return(expr) :: Nil
      case _                        => compiledStmts ::: JavaAst.Return(unitInstance) :: Nil
    }
    val funcType = types(l.id).tpe.asInstanceOf[FuncType]
    JavaAst.Lambda(
      compileType(funcType),
      l.args.zipWithIndex.map { case (arg, idx) => JavaAst.Arg(arg.name, compileType(funcType.args(idx))) },
      compileType(funcType.out),
      body
    )
  }

  def compileStruct(name: String, tpe: StructType): JavaAst.Struct = {
    JavaAst.Struct(
      name,
      tpe.args.map { case (argName, tpe) => JavaAst.Arg(argName, compileType(tpe)) }
    )
  }

  private def compileExpr(
      expr: Ast.Expr,
      types: TypeInterpreter.TypeCache
  ): CompiledExpr = {
    expr match {
      case l: Ast.Literal =>
        l.value match {
          case i: Int     => ce(JavaAst.IntLiteral(i))
          case s: String  => ce(JavaAst.StrLiteral(s))
          case b: Boolean => ce(JavaAst.BoolLiteral(b))
        }
      case Ast.Ident(_, name) => ce(JavaAst.Ident(name))
      case Ast.Call(_, expr, args, _) =>
        val compiledArgs = compileExprs(args, types)
        val stmts = compiledArgs.flatMap(_.stmts)
        val exprs = compiledArgs.map(_.expr)

        val calleeTpe = types(expr.id)
        calleeTpe match {
          case TypedValue(_: FuncType, _, fullName, _) =>
            val calleeExpr = fullName match {
              case Some(fullName) => ce(JavaAst.Ident(fullName))
              case None           => compileExpr(expr, types)
            }
            CompiledExpr(stmts, JavaAst.Call(calleeExpr.expr, exprs))
          case TypedValue(TypeType, Some(s: StructType), _, _) =>
            CompiledExpr(stmts, JavaAst.New(compileType(s), exprs))
        }

      case i: Ast.If =>
        compileExprs(List(i.expr, i.success, i.fail), types) match {
          case compiledCond :: compiledSuccess :: compiledFail :: Nil =>
            if (compiledSuccess.stmts.isEmpty && compiledFail.stmts.isEmpty)
              CompiledExpr(
                compiledCond.stmts,
                JavaAst.IfExpr(compiledCond.expr, compiledSuccess.expr, compiledFail.expr)
              )
            else {
              val resName = "ifRes$" + nextId()
              val decl = JavaAst.Decl(resName, compileType(types(i.id).tpe))
              val ifStmt = JavaAst.IfStmt(
                compiledCond.expr,
                compiledSuccess.stmts ::: JavaAst.Set(resName, compiledSuccess.expr) :: Nil,
                compiledFail.stmts ::: JavaAst.Set(resName, compiledFail.expr) :: Nil
              )
              CompiledExpr(decl :: ifStmt :: Nil, JavaAst.Ident(resName))
            }
        }
      case b: Ast.Block =>
        b.stmts match {
          case Nil                     => ce(unitInstance)
          case (head: Ast.Expr) :: Nil => compileExpr(head, types)
          case (head: Ast.Val) :: Nil =>
            val compiled = compileExpr(head.expr, types)
            CompiledExpr(compiled.stmts ::: compiled.expr :: Nil, unitInstance)
          case _ =>
            val compiled = compileStmts(b.stmts, types)
            compiled.lastOption match {
              case Some(e: JavaAst.Expr) =>
                val resName = "blockRes$" + nextId()
                val decl = JavaAst.Decl(resName, compileType(types(b.id).tpe))
                val javaBlock = JavaAst.Block(compiled.take(compiled.length - 1) ::: JavaAst.Set(resName, e) :: Nil)
                CompiledExpr(decl :: javaBlock :: Nil, JavaAst.Ident(resName))
              case _ => CompiledExpr(compiled, unitInstance)
            }
        }
      case Ast.MemberRef(_, expr, memberName) =>
        val compiled = compileExpr(expr, types)
        CompiledExpr(compiled.stmts, JavaAst.MemberRef(compiled.expr, memberName))
      case l: Ast.Lambda => ce(compileLambda(l, types))

    }
  }

  private def compileStmt(
      stmt: Ast.FnBodyStmt,
      types: TypeInterpreter.TypeCache
  ): List[JavaAst.Stmt] =
    stmt match {
      case Ast.Val(_, name, expr) =>
        val compiled = compileExpr(expr, types)
        compiled.stmts ::: JavaAst.Let(name, compileType(types(expr.id).tpe), compiled.expr) :: Nil
      case expr: Ast.Expr => compileExpr(expr, types).asStmts
    }

  private def getAllStructTypes(cache: TypeCache): List[(StructType, Option[String])] = {
    val thisLevel = cache.values.collect { case TypedValue(TypeType, Some(t: StructType), name, _) => t -> name }.toList
    val nextLevelCaches = cache.values.collect { case TypedValue(ComptimeFunc, Some(f: CachingFunc), _, _) =>
      f.cache.toList.map(_._2._1)
    }.flatten
    thisLevel ::: nextLevelCaches.toList.flatMap(getAllStructTypes)
  }

  def compile(p: Ast.FlatProgram): JavaAst.Program = {
    val types = TypeInterpreter.typeAll(p)

    var idCounter = 0

    val allStructTypes = getAllStructTypes(types)
    val namedStructTypes = allStructTypes
      .groupBy(_._1)
      .map { case (s, l) =>
        s -> l.map(_._2).find(_.isDefined).flatten.getOrElse {
          idCounter += 1
          "Struct$" + idCounter
        }
      }
      .toList
      .sortBy(_._2)
    namedStructTypes.foreach { case (s, n) => structNameCache.put(s, n) }

    val javaStructs = namedStructTypes.map { case (st, name) => compileStruct(name, st) }

    val javaVals = p.stmts
      .collect { case v: Ast.Val => v }
      .filter(v => !types(v.expr.id).isComptime) // We compiled structs separately above
      .map { v =>
        val compiled = compileExpr(v.expr, types)
        val exprType = types(v.expr.id).tpe
        val resType = compileType(exprType)

        val resExpr =
          if (compiled.stmts.isEmpty) compiled.expr
          else {
            JavaAst.Call(
              JavaAst.Lambda(
                compileType(FuncType(Nil, exprType)),
                Nil,
                resType,
                compiled.stmts :+ JavaAst.Return(compiled.expr)
              ),
              Nil
            )
          }

        JavaAst.StaticLet(v.name, resType, resExpr)
      }

    JavaAst.Program(javaStructs ::: javaVals)
  }
}

case class CompiledExpr(stmts: List[JavaAst.Stmt], expr: JavaAst.Expr) {
  def asStmts: List[JavaAst.Stmt] = stmts ::: expr :: Nil
}
