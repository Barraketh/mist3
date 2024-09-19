package com.mistlang.lang

import com.mistlang.lang.Ast._

object RemoveShadowing {
  private def rewriteExpr[A <: Expr](expr: A): State[NameTracker, A] = {
    (expr match {
      case l: Literal      => State[NameTracker, Literal](tracker => (l, tracker))
      case Ident(id, name) => State[NameTracker, Ident](tracker => (Ident(id, tracker.getIdent(name)), tracker))
      case c: Call =>
        for {
          newFunc <- rewriteExpr(c.func)
          newArgs <- State.traverse[NameTracker, Expr, Expr](c.args, rewriteExpr)
        } yield c.copy(func = newFunc, args = newArgs)
      case m: MemberRef => rewriteExpr(m.expr).map(newExpr => m.copy(expr = newExpr))
      case If(id, cond, succ, fail) =>
        for {
          newCond <- rewriteExpr(cond)
          newSucc <- rewriteExpr(succ)
          newFail <- rewriteExpr(fail)
        } yield If(id, newCond, newSucc, newFail)
      case Block(id, stmts) => rewriteStmts(stmts).map(newStmts => Block(id, newStmts))
      case l: Lambda =>
        State[NameTracker, Lambda] { tracker =>
          val newTracker = tracker.declareAll(l.args.map(_.name))
          // Todo: shadowing in types?
          rewriteExpr(l.body).map(newBody => l.copy(body = newBody)).run(newTracker)
        }
      case s: Struct => State[NameTracker, Struct](tracker => (s, tracker))
    }).asInstanceOf[State[NameTracker, A]]
  }
  private def rewriteStmt[A <: FnBodyStmt](stmt: A): State[NameTracker, A] = {
    (stmt match {
      case Val(id, name, expr) =>
        State[NameTracker, Val] { tracker =>
          val (newName, tracker1) = tracker.declareName(name)
          val (newExpr, tracker2) = rewriteExpr(expr).run(tracker1)
          val finalTracker = if (name == newName) tracker2 else tracker2.rename(name, newName)
          (Val(id, newName, newExpr), finalTracker)
        }
      case e: Expr => rewriteExpr(e)
    }).asInstanceOf[State[NameTracker, A]]
  }
  private def rewriteStmts[A <: FnBodyStmt](stmts: List[A]): State[NameTracker, List[A]] =
    State.traverse[NameTracker, A, A](stmts, rewriteStmt)
  def removeShadowing(p: FlatProgram): FlatProgram = {
    val emptyTracker = NameTracker(Set.empty, Map.empty)
    val valNames = p.stmts.map(_.name)
    val tracker = emptyTracker.declareAll(valNames)

    val newStmts = p.stmts.map(v => Val(v.id, v.name, rewriteExpr(v.expr).runA(tracker)))
    FlatProgram(newStmts)
  }

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

    def declareAll(names: List[String]): NameTracker = names.foldLeft(this) { case (curTracker, nextName) =>
      curTracker.declareName(nextName)._2
    }

    def rename(oldName: String, newName: String): NameTracker = {
      if (!declared.contains(oldName)) throw new RuntimeException(s"Should not be renaming - ${oldName} not declared")
      if (!declared.contains(newName)) throw new RuntimeException(s"New name - ${newName} not declared")
      NameTracker(declared, renamed + (oldName -> newName))
    }

    def getIdent(name: String) = renamed.getOrElse(name, name)
  }

}
