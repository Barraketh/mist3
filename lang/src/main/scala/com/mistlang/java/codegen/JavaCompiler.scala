package com.mistlang.java.codegen

import com.mistlang.lang.Types._
import com.mistlang.lang.{Ast, Type, Typer}

object JavaCompiler {
  val unitInstance = JavaAst.Ident("Unit.unit")

  case class NameTracker(declared: Set[String], renamed: Map[String, String]) {
    private def nextName(name: String): String = {
      val next = name + "$"
      if (!renamed.contains(next)) next
      else nextName(next)
    }
    def declareName(name: String) = {
      if (!declared.contains(name)) (name, NameTracker(declared + name, renamed))
      else {
        val next = nextName(name)
        (next, NameTracker(declared + next, renamed))
      }
    }

    def rename(oldName: String, newName: String): NameTracker = {
      if (!declared.contains(oldName)) throw new RuntimeException(s"Should not be renaming - ${oldName} not declared")
      if (!declared.contains(newName)) throw new RuntimeException(s"New name - ${newName} not declared")
      NameTracker(declared, renamed + (oldName -> newName))
    }

  }
  object NameTracker {
    val empty = NameTracker(Set.empty, Map.empty)
  }

  case class CompiledExpr(stmts: List[JavaAst.Stmt], expr: JavaAst.Expr) {
    def asStmts: List[JavaAst.Stmt] = stmts ::: expr :: Nil
  }

  private def ce(expr: JavaAst.Expr) = CompiledExpr(Nil, expr)

  private def compileExprs(
      exprs: List[Ast.Expr],
      types: Typer.TypeCache,
      nameTracker: NameTracker
  ): (List[CompiledExpr], NameTracker) = {
    exprs.foldLeft((Nil: List[CompiledExpr], nameTracker)) { case ((curCompiled, curTracker), curExpr) =>
      val (compiledExpr, nextTracker) = compileExpr(curExpr, types, curTracker)
      (curCompiled ::: compiledExpr :: Nil, nextTracker)
    }
  }

  private def compileStmts(
      stmts: List[Ast.Stmt],
      types: Typer.TypeCache,
      nameTracker: NameTracker
  ): (List[JavaAst.Stmt], NameTracker) = {
    stmts.foldLeft((Nil: List[JavaAst.Stmt], nameTracker)) { case ((curCompiled, curTracker), curStmt) =>
      val (compiledStmt, nextTracker) = compileStmt(curStmt, types, curTracker)
      (curCompiled ::: compiledStmt, nextTracker)
    }
  }

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType                   => "Integer"
      case StrType                   => "String"
      case BoolType                  => "Boolean"
      case UnitType                  => "Unit"
      case AnyType                   => "Object"
      case StructType(name, path, _) => if (path.isEmpty) name else s"$path.$name"
      case f: FuncType               => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: FuncType): String = {
    val argTypes = func.args.map(compileType)
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  def compileDef(d: Ast.Def, types: Typer.TypeCache): JavaAst.Def = {
    val compiledStmts = d.lambda.body match {
      case b: Ast.Block => compileStmts(b.stmts, types, NameTracker.empty)._1
      case other        => compileExpr(other, types, NameTracker.empty)._1.asStmts
    }
    val body = compiledStmts.lastOption match {
      case Some(expr: JavaAst.Expr) => compiledStmts.take(compiledStmts.length - 1) ::: JavaAst.Return(expr) :: Nil
      case _                        => compiledStmts ::: JavaAst.Return(unitInstance) :: Nil
    }
    val funcType = types(d.lambda.id).asInstanceOf[FuncType]
    JavaAst.Def(
      compileType(funcType),
      d.name,
      d.lambda.args.zipWithIndex.map { case (arg, idx) => JavaAst.Arg(arg.name, compileType(funcType.args(idx))) },
      compileType(funcType.out),
      body
    )
  }

  def compileStruct(s: Ast.Struct, types: Typer.TypeCache): JavaAst.Struct = {
    JavaAst.Struct(s.name, s.args.map(arg => JavaAst.Arg(arg.name, compileType(types(arg.tpe.id)))))
  }

  def compileExpr(expr: Ast.Expr, types: Typer.TypeCache, nameTracker: NameTracker): (CompiledExpr, NameTracker) = {
    expr match {
      case l: Ast.Literal =>
        l.value match {
          case i: Int     => (ce(JavaAst.IntLiteral(i)), nameTracker)
          case s: String  => (ce(JavaAst.StrLiteral(s)), nameTracker)
          case b: Boolean => (ce(JavaAst.BoolLiteral(b)), nameTracker)
        }
      case Ast.Ident(_, name) =>
        val newName = name match {
          case "+"  => "plusOp"
          case "-"  => "minusOp"
          case "*"  => "productOp"
          case "==" => "eqIntOp"
          case _ =>
            nameTracker.renamed.get(name) match {
              case Some(n) => n
              case None    => name
            }
        }
        (ce(JavaAst.Ident(newName)), nameTracker)
      case Ast.Call(_, expr, args, _) =>
        val (compiled, newTracker) = compileExprs(expr :: args, types, nameTracker)
        val stmts = compiled.flatMap(_.stmts)
        val exprs = compiled.map(_.expr)
        val calleeTpe = types(expr.id)

        calleeTpe match {
          case _: FuncType =>
            (CompiledExpr(stmts, JavaAst.Call(exprs.head, exprs.tail)), newTracker)
          case s: StructType =>
            (CompiledExpr(stmts, JavaAst.New(compileType(s), exprs.tail)), newTracker)
        }

      case i: Ast.If =>
        compileExprs(List(i.expr, i.success, i.fail), types, nameTracker) match {
          case (compiledCond :: compiledSuccess :: compiledFail :: Nil, tracker1) =>
            if (compiledSuccess.stmts.isEmpty && compiledFail.stmts.isEmpty)
              (
                CompiledExpr(
                  compiledCond.stmts,
                  JavaAst.IfExpr(compiledCond.expr, compiledSuccess.expr, compiledFail.expr)
                ),
                tracker1
              )
            else {
              val (resName, tracker2) = tracker1.declareName("ifRes$")
              val decl = JavaAst.Decl(resName, compileType(types(i.id)))
              val ifStmt = JavaAst.IfStmt(
                compiledCond.expr,
                compiledSuccess.stmts ::: JavaAst.Set(resName, compiledSuccess.expr) :: Nil,
                compiledFail.stmts ::: JavaAst.Set(resName, compiledFail.expr) :: Nil
              )
              (CompiledExpr(decl :: ifStmt :: Nil, JavaAst.Ident(resName)), tracker2)
            }
        }
      case b: Ast.Block =>
        b.stmts match {
          case Nil                     => (ce(unitInstance), nameTracker)
          case (head: Ast.Expr) :: Nil => compileExpr(head, types, nameTracker)
          case (head: Ast.Val) :: Nil =>
            val (compiled, nextTracker) = compileExpr(head.expr, types, nameTracker)
            (CompiledExpr(compiled.stmts ::: compiled.expr :: Nil, unitInstance), nextTracker)
          case _ =>
            val (compiled, tracker1) = compileStmts(b.stmts, types, nameTracker)
            compiled.lastOption match {
              case Some(e: JavaAst.Expr) =>
                val (resName, tracker2) = nameTracker.declareName("blockRes$")
                val decl = JavaAst.Decl(resName, compileType(types(b.id)))
                val javaBlock = JavaAst.Block(compiled.take(compiled.length - 1) ::: JavaAst.Set(resName, e) :: Nil)
                (CompiledExpr(decl :: javaBlock :: Nil, JavaAst.Ident(resName)), tracker2)
              case _ => (CompiledExpr(compiled, unitInstance), tracker1)
            }
        }
      case Ast.MemberRef(_, expr, memberName) =>
        val (compiled, nextTracker) = compileExpr(expr, types, nameTracker)
        (CompiledExpr(compiled.stmts, JavaAst.MemberRef(compiled.expr, memberName)), nextTracker)
//      case Ast.New(args, tpe) =>
//        val (compiledArgs, nextTracker) = compileExprs(args, nameTracker)
//        (
//          CompiledExpr(compiledArgs.flatMap(_.stmts), JavaAst.New(compileType(tpe), compiledArgs.map(_.expr))),
//          nextTracker
//        )

    }
  }

  def compileStmt(stmt: Ast.Stmt, types: Typer.TypeCache, nameTracker: NameTracker): (List[JavaAst.Stmt], NameTracker) =
    stmt match {
      case Ast.Val(name, expr) =>
        val (newName, tracker1) = nameTracker.declareName(name)
        val (compiled, tracker2) = compileExpr(expr, types, tracker1)
        val finalTracker = if (name == newName) tracker2 else tracker2.rename(name, newName)
        (compiled.stmts ::: JavaAst.Let(newName, compileType(types(expr.id)), compiled.expr) :: Nil, finalTracker)
      case expr: Ast.Expr =>
        val (compiled, nextTracker) = compileExpr(expr, types, nameTracker)
        (compiled.asStmts, nextTracker)

    }

  private def compileTopLevel(stmt: Ast.TopLevelStmt, types: Typer.TypeCache): JavaAst.TopLevelStmt = stmt match {
    case s: Ast.Struct    => compileStruct(s, types)
    case d: Ast.Def       => compileDef(d, types)
    case n: Ast.Namespace => JavaAst.Namespace(n.name, n.children.map(s => compileTopLevel(s, types)))
  }

  private def childExprs(stmt: Ast.TopLevelStmt): List[Ast.Expr] = stmt match {
    case Ast.Def(lambda)            => lambda :: Nil
    case Ast.Struct(_, _, args)        => args.map(_.tpe)
    case Ast.Namespace(_, children) => children.flatMap(c => childExprs(c))
  }

  private def childExprs(expr: Ast.Stmt): List[Ast.Expr] = expr match {
    case v: Ast.Val                    => v.expr :: Nil
    case e: Ast.Call                   => e.func :: e.args
    case e: Ast.If                     => List(e.expr, e.success, e.fail)
    case e: Ast.MemberRef              => List(e.expr)
    case e: Ast.Block                  => e.stmts.flatMap(childExprs)
    case e: Ast.Lambda                 => e.body :: e.args.map(_.tpe) ::: e.outType.toList
    case _: Ast.Literal | _: Ast.Ident => Nil

  }

  private def maxExprId(expr: Ast.Expr): Int = {
    val childIds = childExprs(expr).map(maxExprId)
    (expr.id :: childIds).max
  }

  private def maxId(ast: Ast.Ast): Int = {
    ast match {
      case stmt: Ast.TopLevelStmt => childExprs(stmt).map(maxExprId).max
      case v: Ast.Val             => maxExprId(v.expr)
      case e: Ast.Expr            => maxExprId(e)
    }
  }

  def compile(p: Ast.Program): JavaAst.Program = {
    val curMaxId = (p.topLevelStmts ::: p.stmts).map(maxId).max
    val runFunc = Ast.Lambda(curMaxId + 1, Some("run"), Nil, None, Ast.Block(curMaxId + 2, p.stmts))
    val p2 = Ast.Program(Ast.Def(runFunc) :: p.topLevelStmts, Nil)

    val types = Typer.typeAll(p2)

    JavaAst.Program(p2.topLevelStmts.map(s => compileTopLevel(s, types)))
  }
}
