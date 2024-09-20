package com.mistlang.java.codegen

import com.mistlang.lang
import com.mistlang.lang.ComptimeValue.{CachingFunc, SimpleValue}
import com.mistlang.lang.FastparseParser.IdProvider
import com.mistlang.lang.TypeInterpreter.{TypeCache, error}
import com.mistlang.lang.Types._
import com.mistlang.lang._

class JavaCompiler {
  private val unitInstance = JavaAst.Ident("Unit.unit")

  private val structNameCache = scala.collection.mutable.Map[StructType, String]()

  private var curId = 0
  private def nextJavaId(): Int = {
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
    val exprTypedValue = types(expr.id)
    exprTypedValue.name match {
      case Some(name) => ce(JavaAst.Ident(name))
      case None =>
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
              case TypedValue(_: FuncType, _, _, _) =>
                val calleeExpr = compileExpr(expr, types)
                CompiledExpr(stmts, JavaAst.Call(calleeExpr.expr, exprs))
              case TypedValue(TypeType, Some(s: StructType), _, _) =>
                CompiledExpr(stmts, JavaAst.New(compileType(s), exprs))
            }

          case i: Ast.If =>
            types(i.expr.id).value match {
              case Some(SimpleValue(true))  => compileExpr(i.success, types)
              case Some(SimpleValue(false)) => compileExpr(i.fail, types)
              case _ =>
                compileExprs(List(i.expr, i.success, i.fail), types) match {
                  case compiledCond :: compiledSuccess :: compiledFail :: Nil =>
                    if (compiledSuccess.stmts.isEmpty && compiledFail.stmts.isEmpty)
                      CompiledExpr(
                        compiledCond.stmts,
                        JavaAst.IfExpr(compiledCond.expr, compiledSuccess.expr, compiledFail.expr)
                      )
                    else {
                      val resName = "ifRes$" + nextJavaId()
                      val decl = JavaAst.Decl(resName, compileType(types(i.id).tpe))
                      val ifStmt = JavaAst.IfStmt(
                        compiledCond.expr,
                        compiledSuccess.stmts ::: JavaAst.Set(resName, compiledSuccess.expr) :: Nil,
                        compiledFail.stmts ::: JavaAst.Set(resName, compiledFail.expr) :: Nil
                      )
                      CompiledExpr(decl :: ifStmt :: Nil, JavaAst.Ident(resName))
                    }
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
                    val resName = "blockRes$" + nextJavaId()
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

  private def typedValueToExpr(t: TypedValue, nextId: IdProvider): Ast.Expr = {
    t.name
      .map(name => Ast.Ident(nextId(), name))
      .orElse {
        t.value.map {
          case ComptimeValue.SimpleValue(value) => Ast.Literal(nextId(), value)
          case other                            => error(s"Cannot pass $other as arg to comptime fn")
        }
      }
      .get
  }

  private def getAllVals(vals: List[Ast.Val], cache: TypeCache, nextId: IdProvider): List[(Ast.Val, TypeCache)] = {
    vals.flatMap { v =>
      val tv = cache(v.expr.id)
      tv match {
        case TypedValue(ComptimeFunc, Some(c: CachingFunc), _, _) =>
          val lambda = v.expr.asInstanceOf[Ast.Lambda]
          c.cache.toList.map { case (args, (curCache, curValue)) =>
            val blockId = nextId()
            curCache.put(blockId, curValue.copy(name = None))
            val expr = Ast.Block(
              blockId,
              lambda.args.map(_.name).zip(args).filter(!_._2.isComptime).map { case (name, tv) =>
                val expr = typedValueToExpr(tv, nextId)
                curCache.put(expr.id, tv)
                Ast.Val(nextId(), name, expr)
              } ::: lambda.body :: Nil
            )
            (Ast.Val(v.id, curValue.name.get, expr), cache ++ curCache)
          }
        case _ => List(v -> cache)
      }
    }
  }

  def compile(p: Ast.FlatProgram, nextId: IdProvider): JavaAst.Program = {
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

    val allVals = getAllVals(p.stmts, types, nextId)
    val javaVals = allVals
      .filter { case (v, types) => !types(v.expr.id).isComptime } // We compiled structs separately above
      .map { case (v, types) =>
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
