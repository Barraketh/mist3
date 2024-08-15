package com.mistlang.java.codegen

import com.mistlang.lang
import com.mistlang.lang.Types._
import com.mistlang.lang.{Ast, Type, TypeInterpreter, TypedValue}

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
      types: lang.TypeInterpreter.TypeCache,
      nameTracker: NameTracker
  ): (List[CompiledExpr], NameTracker) = {
    exprs.foldLeft((Nil: List[CompiledExpr], nameTracker)) { case ((curCompiled, curTracker), curExpr) =>
      val (compiledExpr, nextTracker) = compileExpr(curExpr, types, curTracker)
      (curCompiled ::: compiledExpr :: Nil, nextTracker)
    }
  }

  private def compileStmts(
      stmts: List[Ast.Stmt],
      types: TypeInterpreter.TypeCache,
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
      case ArrayType(underlying)     => compileType(underlying) + "[]"
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

  def compileLambda(l: Ast.Lambda, types: TypeInterpreter.TypeCache): JavaAst.Lambda = {
    val compiledStmts = l.body match {
      case b: Ast.Block => compileStmts(b.stmts, types, NameTracker.empty)._1
      case other        => compileExpr(other, types, NameTracker.empty)._1.asStmts
    }
    val body = compiledStmts.lastOption match {
      case Some(expr: JavaAst.Expr) => compiledStmts.take(compiledStmts.length - 1) ::: JavaAst.Return(expr) :: Nil
      case _                        => compiledStmts ::: JavaAst.Return(unitInstance) :: Nil
    }
    val funcType = types(l.id.get).tpe.asInstanceOf[FuncType]
    JavaAst.Lambda(
      compileType(funcType),
      l.args.zipWithIndex.map { case (arg, idx) => JavaAst.Arg(arg.name, compileType(funcType.args(idx))) },
      compileType(funcType.out),
      body
    )
  }

  def compileDef(d: Ast.Def, types: TypeInterpreter.TypeCache): JavaAst.Def = {
    val lambda = compileLambda(d.lambda, types)
    JavaAst.Def(d.name, lambda)
  }

  def compileStruct(s: Ast.Struct, types: lang.TypeInterpreter.TypeCache): JavaAst.Struct = {
    JavaAst.Struct(
      s.name,
      s.args.map(arg => JavaAst.Arg(arg.name, compileType(types(arg.tpe.id.get).value.get.asInstanceOf[Type])))
    )
  }

  def compileExpr(
      expr: Ast.Expr,
      types: TypeInterpreter.TypeCache,
      nameTracker: NameTracker
  ): (CompiledExpr, NameTracker) = {
    expr match {
      case l: Ast.Literal =>
        l.value match {
          case i: Int     => (ce(JavaAst.IntLiteral(i)), nameTracker)
          case s: String  => (ce(JavaAst.StrLiteral(s)), nameTracker)
          case b: Boolean => (ce(JavaAst.BoolLiteral(b)), nameTracker)
        }
      case Ast.Ident(_, name) =>
        val newName = nameTracker.renamed.get(name) match {
          case Some(n) => n
          case None    => name
        }
        (ce(JavaAst.Ident(newName)), nameTracker)
      case Ast.Call(_, expr, args, _) =>
        val (compiledArgs, newTracker) = compileExprs(args, types, nameTracker)
        val stmts = compiledArgs.flatMap(_.stmts)
        val exprs = compiledArgs.map(_.expr)

        val calleeTpe = types(expr.id.get)
        calleeTpe match {
          case TypedValue(f: FuncType, _) =>
            val (calleeExpr, newTracker2) = if (f.fullName.isEmpty) {
              compileExpr(expr, types, newTracker)
            } else {
              (ce(JavaAst.Ident(f.fullName)), nameTracker)
            }
            (CompiledExpr(stmts, JavaAst.Call(calleeExpr.expr, exprs)), newTracker2)
          case TypedValue(TypeType, Some(s: StructType)) =>
            (CompiledExpr(stmts, JavaAst.New(compileType(s), exprs)), newTracker)
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
              val decl = JavaAst.Decl(resName, compileType(types(i.id.get).tpe))
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
                val decl = JavaAst.Decl(resName, compileType(types(b.id.get).tpe))
                val javaBlock = JavaAst.Block(compiled.take(compiled.length - 1) ::: JavaAst.Set(resName, e) :: Nil)
                (CompiledExpr(decl :: javaBlock :: Nil, JavaAst.Ident(resName)), tracker2)
              case _ => (CompiledExpr(compiled, unitInstance), tracker1)
            }
        }
      case Ast.MemberRef(_, expr, memberName) =>
        val (compiled, nextTracker) = compileExpr(expr, types, nameTracker)
        (CompiledExpr(compiled.stmts, JavaAst.MemberRef(compiled.expr, memberName)), nextTracker)
      case m: Ast.MethodRef =>
        types(m.id.get) match {
          case TypedValue(f: FuncType, _) => (ce(JavaAst.Ident(f.fullName)), nameTracker)
        }
      case l: Ast.Lambda => (ce(compileLambda(l, types)), nameTracker)

    }
  }

  def compileStmt(
      stmt: Ast.Stmt,
      types: TypeInterpreter.TypeCache,
      nameTracker: NameTracker
  ): (List[JavaAst.Stmt], NameTracker) =
    stmt match {
      case Ast.Val(name, expr) =>
        val (newName, tracker1) = nameTracker.declareName(name)
        val (compiled, tracker2) = compileExpr(expr, types, tracker1)
        val finalTracker = if (name == newName) tracker2 else tracker2.rename(name, newName)
        (
          compiled.stmts ::: JavaAst.Let(newName, compileType(types(expr.id.get).tpe), compiled.expr) :: Nil,
          finalTracker
        )
      case expr: Ast.Expr =>
        val (compiled, nextTracker) = compileExpr(expr, types, nameTracker)
        (compiled.asStmts, nextTracker)

    }

  private def compileTopLevel(stmt: Ast.TopLevelStmt, types: TypeInterpreter.TypeCache): JavaAst.TopLevelStmt =
    stmt match {
      case s: Ast.Struct    => compileStruct(s, types)
      case d: Ast.Def       => compileDef(d, types)
      case n: Ast.Namespace => JavaAst.Namespace(n.name, n.children.map(s => compileTopLevel(s, types)))
    }

  val maxExprId: Int = 10_000_000

  def compile(p: Ast.Program): JavaAst.Program = {
    val runFunc = Ast.Lambda(Some(maxExprId + 1), Some("run"), Nil, None, Ast.Block(Some(maxExprId + 2), p.stmts))
    val p2 = Ast.Program(Ast.Def(runFunc) :: p.topLevelStmts, Nil)

    val types = TypeInterpreter.typeAll(p2)

    JavaAst.Program(p2.topLevelStmts.map(s => compileTopLevel(s, types)))
  }
}
