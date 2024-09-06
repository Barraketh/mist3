package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast.{FlatProgram, Program}

class NamespaceResolver {
  import NamespaceResolver._

  type MyEnvType = Env[RuntimeValue[EnvValue]]
  val cache = collection.mutable.Map[Int, EnvValue]()

  import TypeInterpreter.error
  private def evalExpr(env: MyEnvType, exp: Ast.Expr)(implicit namespace: String): EnvValue = {
    val res: EnvValue = exp match {
      case _: Ast.Literal => Local
      case Ast.Ident(_, name) =>
        env.get(name).map(_.value).getOrElse(Local) // Assume that don't need to rewrite references to unknown names
      case Ast.Call(_, func, args, _) =>
        (func :: args).foreach(e => evalExpr(env, e))
        Local
      case Ast.MemberRef(_, expr, memberName) =>
        val ref = evalExpr(env, expr)
        ref match {
          case n: Namespace =>
            n.children.getOrElse(memberName, error(s"$memberName not found in namespace ${n.fullName}"))
          case _ => Local
        }
      case Ast.If(_, cond, succ, fail) =>
        List(cond, succ, fail).foreach(e => evalExpr(env, e))
        Local
      case Ast.Lambda(id, name, args, outType, body) =>
        args.foreach(arg => evalExpr(env, arg.tpe))
        outType.foreach(out => evalExpr(env, out))

        val res = name match {
          case Some(name) => FlatValue(Env.fullName(namespace, name))
          case None       => Local
        }

        val newEnv = args.foldLeft(env.newScope) { case (curEnv, nextArg) => curEnv.put(nextArg.name, Strict(Local)) }
        val withRec = name match {
          case Some(name) => newEnv.put(name, Strict(res))
          case None       => newEnv
        }

        evalExpr(withRec, body)
        res
      case Ast.Block(_, stmts) => runAll(env.newScope, stmts)._2
    }

    cache.put(exp.id, res)
    res
  }

  private def run(env: MyEnvType, stmt: Ast.Stmt)(implicit namespace: String): (MyEnvType, EnvValue) = {
    stmt match {
      case expr: Ast.Expr => (env, evalExpr(env, expr))
      case Ast.Val(name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, Strict(evaluated)), Local)
    }
  }

  private def runAll(env: MyEnvType, stmts: List[Ast.Stmt])(implicit namespace: String): (MyEnvType, EnvValue) = {
    stmts.foldLeft((env, Local: EnvValue)) { case ((curEnv, _), nextStmt) => run(curEnv, nextStmt) }
  }

  private def evalStruct(env: MyEnvType, s: Ast.Struct)(implicit namespace: String): EnvValue = {
    s.args.foreach(a => evalExpr(env, a.tpe))
    val res = FlatValue(Env.fullName(namespace, s.name))
    cache.put(s.id, res)
    res
  }

  private def evalNamespace(env: MyEnvType, n: Ast.Namespace)(implicit namespace: String): EnvValue = {
    val newNamespaceName: String = Env.fullName(namespace, n.name)
    val namespaceEnv = runAllTopLevel(env.newScope, n.children)(newNamespaceName)
    val names = n.children.map(_.name)
    Namespace(newNamespaceName, names.map(name => name -> namespaceEnv.get(name).get.value).toMap)
  }

  private def runTopLevel(env: MyEnvType, stmt: Ast.TopLevelStmt)(implicit namespace: String): Unit = {
    stmt match {
      case d: Ast.Def       => env.set(d.name, Lazy(() => evalExpr(env, d.lambda)))
      case s: Ast.Struct    => env.set(s.name, Lazy(() => evalStruct(env, s)))
      case n: Ast.Namespace => env.set(n.name, Lazy(() => evalNamespace(env, n)))
    }
  }

  private def runAllTopLevel(env: MyEnvType, stmts: List[Ast.TopLevelStmt])(implicit namespace: String): MyEnvType = {
    val newEnv = stmts.map(_.name).foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }
    stmts.foreach(stmt => runTopLevel(newEnv, stmt))
    newEnv
  }

  private def rewriteNames(expr: Ast.Expr): Ast.Expr = {
    cache.get(expr.id) match {
      case Some(FlatValue(name)) if !expr.isInstanceOf[Ast.Lambda] => Ast.Ident(expr.id, name)
      case _ =>
        expr match {
          case e: Ast.Literal => e
          case e: Ast.Ident   => e
          case Ast.Call(id, func, args, isInfixCall) =>
            Ast.Call(id, rewriteNames(func), args.map(rewriteNames), isInfixCall)
          case Ast.MemberRef(id, expr, memberName) => Ast.MemberRef(id, rewriteNames(expr), memberName)
          case Ast.If(id, expr, success, fail) =>
            Ast.If(id, rewriteNames(expr), rewriteNames(success), rewriteNames(fail))
          case Ast.Block(id, stmts) => Ast.Block(id, rewriteNames(stmts))
          case Ast.Lambda(id, name, args, outType, body) =>
            val newName = name.map(_ => cache(id).fullName)
            val newArgs = args.map(arg => Ast.ArgDecl(arg.name, rewriteNames(arg.tpe)))
            val newOutType = outType.map(rewriteNames)
            val newBody = rewriteNames(body)
            Ast.Lambda(id, newName, newArgs, newOutType, newBody)
        }
    }
  }

  private def rewriteNames(stmts: List[Ast.Stmt]): List[Ast.Stmt] = {
    stmts.map {
      case Ast.Val(name, expr) => Ast.Val(name, rewriteNames(expr))
      case expr: Ast.Expr      => rewriteNames(expr)
    }
  }

  private def flatten(stmts: List[Ast.TopLevelStmt]): List[Ast.FlatTopLevelStmt] = {
    stmts.flatMap {
      case d: Ast.Def => List(Ast.Def(rewriteNames(d.lambda).asInstanceOf[Ast.Lambda]))
      case s: Ast.Struct =>
        val newName = cache(s.id).fullName
        val newArgs = s.args.map(arg => Ast.ArgDecl(arg.name, rewriteNames(arg.tpe)))
        List(Ast.Struct(s.id, newName, s.typeArgs, newArgs))
      case n: Ast.Namespace => flatten(n.children)
    }
  }

  def resolveNamespaces(env: MyEnvType, p: Program): FlatProgram = {
    implicit val namespace: String = ""
    val nextEnv = runAllTopLevel(env, p.topLevelStmts)
    p.topLevelStmts.foreach(s => nextEnv.get(s.name).foreach(_.value))
    runAll(nextEnv, p.stmts)

    val flatTopLevel = flatten(p.topLevelStmts)
    val newStmts = rewriteNames(p.stmts)
    FlatProgram(flatTopLevel, newStmts)
  }
}

object NamespaceResolver {
  sealed trait EnvValue {
    def fullName: String
  }
  case class FlatValue(fullName: String) extends EnvValue
  case class Namespace(fullName: String, children: Map[String, EnvValue]) extends EnvValue
  case object Local extends EnvValue {
    override def fullName: String = TypeInterpreter.error("Tried to get full name of local var")
  }

  def resolveNames(p: Program): FlatProgram = {
    val n = new NamespaceResolver
    n.resolveNamespaces(Env.empty[RuntimeValue[EnvValue]], p)
  }
}
