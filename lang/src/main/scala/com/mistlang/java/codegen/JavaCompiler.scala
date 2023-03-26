package com.mistlang.java.codegen

import com.mistlang.lang.Types._
import com.mistlang.lang.{IR, Type}

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
      exprs: List[IR.Expr],
      nameTracker: NameTracker
  ): (List[CompiledExpr], NameTracker) = {
    exprs.foldLeft((Nil: List[CompiledExpr], nameTracker)) { case ((curCompiled, curTracker), curExpr) =>
      val (compiledExpr, nextTracker) = compileExpr(curExpr, curTracker)
      (curCompiled ::: compiledExpr :: Nil, nextTracker)
    }
  }

  private def compileStmts(stmts: List[IR.Stmt], nameTracker: NameTracker): (List[JavaAst.Stmt], NameTracker) = {
    stmts.foldLeft((Nil: List[JavaAst.Stmt], nameTracker)) { case ((curCompiled, curTracker), curStmt) =>
      val (compiledStmt, nextTracker) = compileStmt(curStmt, curTracker)
      (curCompiled ::: compiledStmt, nextTracker)
    }
  }

  private def compileType(tpe: Type): String = {
    tpe match {
      case IntType             => "Integer"
      case StrType             => "String"
      case BoolType            => "Boolean"
      case UnitType            => "Unit"
      case AnyType             => "Object"
      case StructType(name, _) => name
      case f: FuncType         => compileFunctionType(f)
    }
  }

  private def compileFunctionType(func: FuncType): String = {
    val argTypes = func.args.map(compileType)
    val outType = compileType(func.out)

    val genericParams = argTypes :+ outType
    s"Function${argTypes.length}<${genericParams.mkString(", ")}>"
  }

  def compileDef(d: IR.Def): JavaAst.Def = {
    val compiledStmts = d.body match {
      case b: IR.Block => compileStmts(b.stmts, NameTracker.empty)._1
      case other       => compileExpr(other, NameTracker.empty)._1.asStmts
    }
    val body = compiledStmts.lastOption match {
      case Some(expr: JavaAst.Expr) => compiledStmts.take(compiledStmts.length - 1) ::: JavaAst.Return(expr) :: Nil
      case _                        => compiledStmts ::: JavaAst.Return(unitInstance) :: Nil
    }
    JavaAst.Def(
      compileType(d.tpe),
      d.name,
      d.args.zip(d.tpe.args).map { case (argName, argTpe) => JavaAst.Arg(argName, compileType(argTpe)) },
      compileType(d.tpe.out),
      body
    )
  }

  def compileStruct(s: IR.Struct): JavaAst.Struct = {
    JavaAst.Struct(s.tpe.name, s.tpe.args.map(arg => JavaAst.Arg(arg._1, compileType(arg._2))))
  }

  def compileExpr(expr: IR.Expr, nameTracker: NameTracker): (CompiledExpr, NameTracker) =
    expr match {
      case IR.IntLiteral(i)  => (ce(JavaAst.IntLiteral(i)), nameTracker)
      case IR.StrLiteral(s)  => (ce(JavaAst.StrLiteral(s)), nameTracker)
      case IR.BoolLiteral(b) => (ce(JavaAst.BoolLiteral(b)), nameTracker)
      case IR.Ident(name, _) =>
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
      case IR.Call(expr, args, _) =>
        val (compiled, newTracker) = compileExprs(expr :: args, nameTracker)
        val stmts = compiled.flatMap(_.stmts)
        val exprs = compiled.map(_.expr)
        (CompiledExpr(stmts, JavaAst.Call(exprs.head, exprs.tail)), newTracker)
      case i: IR.If =>
        compileExprs(List(i.expr, i.success, i.fail), nameTracker) match {
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
              val decl = JavaAst.Decl(resName, compileType(i.tpe))
              val ifStmt = JavaAst.IfStmt(
                compiledCond.expr,
                compiledSuccess.stmts ::: JavaAst.Set(resName, compiledSuccess.expr) :: Nil,
                compiledFail.stmts ::: JavaAst.Set(resName, compiledFail.expr) :: Nil
              )
              (CompiledExpr(decl :: ifStmt :: Nil, JavaAst.Ident(resName)), tracker2)
            }
        }
      case b: IR.Block =>
        b.stmts match {
          case Nil                    => (ce(unitInstance), nameTracker)
          case (head: IR.Expr) :: Nil => compileExpr(head, nameTracker)
          case (head: IR.Let) :: Nil =>
            val (compiled, nextTracker) = compileExpr(head.expr, nameTracker)
            (CompiledExpr(compiled.stmts ::: compiled.expr :: Nil, unitInstance), nextTracker)
          case _ =>
            val (compiled, tracker1) = compileStmts(b.stmts, nameTracker)
            compiled.lastOption match {
              case Some(e: JavaAst.Expr) =>
                val (resName, tracker2) = nameTracker.declareName("blockRes$")
                val decl = JavaAst.Decl(resName, compileType(b.tpe))
                val javaBlock = JavaAst.Block(compiled.take(compiled.length - 1) ::: JavaAst.Set(resName, e) :: Nil)
                (CompiledExpr(decl :: javaBlock :: Nil, JavaAst.Ident(resName)), tracker2)
              case _ => (CompiledExpr(compiled, unitInstance), tracker1)
            }
        }
      case IR.MemberRef(expr, memberName, _) =>
        val (compiled, nextTracker) = compileExpr(expr, nameTracker)
        (CompiledExpr(compiled.stmts, JavaAst.MemberRef(compiled.expr, memberName)), nextTracker)
      case IR.New(args, tpe) =>
        val (compiledArgs, nextTracker) = compileExprs(args, nameTracker)
        (
          CompiledExpr(compiledArgs.flatMap(_.stmts), JavaAst.New(compileType(tpe), compiledArgs.map(_.expr))),
          nextTracker
        )

    }

  def compileStmt(stmt: IR.Stmt, nameTracker: NameTracker): (List[JavaAst.Stmt], NameTracker) = stmt match {
    case IR.Let(name, expr) =>
      val (newName, tracker1) = nameTracker.declareName(name)
      val (compiled, tracker2) = compileExpr(expr, tracker1)
      val finalTracker = if (name == newName) tracker2 else tracker2.rename(name, newName)
      (compiled.stmts ::: JavaAst.Let(newName, compileType(expr.tpe), compiled.expr) :: Nil, finalTracker)
    case expr: IR.Expr =>
      val (compiled, nextTracker) = compileExpr(expr, nameTracker)
      (compiled.asStmts, nextTracker)

  }

  def compile(p: IR.Program): JavaAst.Program = {
    val allStmts: List[IR.TopLevelStmt] = p.stmts ::: IR.Def("run", Nil, p.body, FuncType(Nil, p.body.tpe)) :: Nil
    val compiledStmts = allStmts.map {
      case s: IR.Struct => compileStruct(s)
      case d: IR.Def    => compileDef(d)
    }
    JavaAst.Program(compiledStmts)
  }
}
