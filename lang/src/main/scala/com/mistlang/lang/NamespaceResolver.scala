package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast.{FlatProgram, Program}

/** This flattens the program by removing namespaces, and rewriting references accordingly.
  *
  * namespace A { namespace B { def foo(): Int = 1 } }
  *
  * val a = A.B.foo()
  *
  * should be rewritten to
  *
  * def A$B$foo(): Int = 1 val a = A$B$foo()
  *
  * The easiest way to do this correctly in the presence of shadowing is to use Env to track the names
  */
class NamespaceResolver {
  import NamespaceResolver._

  type MyEnvType = Env[RuntimeValue[EnvValue]]
  val cache = collection.mutable.Map[Int, EnvValue]()

  import TypeInterpreter.error
  private def evalExpr(env: MyEnvType, exp: Ast.Expr, name: Option[String] = None)(implicit
      namespace: String
  ): EnvValue = {
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
      case Ast.Lambda(id, args, outType, body, _) =>
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
      case Ast.Struct(_, args) =>
        args.map(_.tpe).foreach(e => evalExpr(env, e))
        Local

    }

    cache.put(exp.id, res)
    res
  }

  private def run(env: MyEnvType, stmt: Ast.FnBodyStmt)(implicit namespace: String): (MyEnvType, EnvValue) = {
    stmt match {
      case expr: Ast.Expr => (env, evalExpr(env, expr))
      case Ast.Val(id, name, expr) =>
        val evaluated = evalExpr(env, expr)
        (env.put(name, Strict(evaluated)), Local)
    }
  }

  private def runAll(env: MyEnvType, stmts: List[Ast.FnBodyStmt])(implicit namespace: String): (MyEnvType, EnvValue) = {
    stmts.foldLeft((env, Local: EnvValue)) { case ((curEnv, _), nextStmt) => run(curEnv, nextStmt) }
  }

  private def evalVal(env: MyEnvType, v: Ast.Val)(implicit namespace: String): EnvValue = {
    evalExpr(env, v.expr, Some(v.name))
    val res = FlatValue(Env.fullName(namespace, v.name))
    cache.put(v.id, res)
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
      case v: Ast.Val       => env.set(v.name, Lazy(() => evalVal(env, v)))
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
          case Ast.Lambda(id, args, outType, body, isComptime) =>
            val newArgs = args.map(arg => Ast.ArgDecl(arg.name, rewriteNames(arg.tpe)))
            val newOutType = outType.map(rewriteNames)
            val newBody = rewriteNames(body)
            Ast.Lambda(id, newArgs, newOutType, newBody, isComptime)
          case s: Ast.Struct =>
            val newArgs = s.args.map { arg => arg.copy(tpe = rewriteNames(arg.tpe)) }
            s.copy(args = newArgs)
        }
    }
  }

  private def rewriteNames(stmts: List[Ast.FnBodyStmt]): List[Ast.FnBodyStmt] = {
    stmts.map {
      case Ast.Val(id, name, expr) => Ast.Val(id, name, rewriteNames(expr))
      case expr: Ast.Expr          => rewriteNames(expr)
    }
  }

  private def flatten(stmts: List[Ast.TopLevelStmt]): List[Ast.Val] = {
    stmts.flatMap {
      case v: Ast.Val =>
        val newName = cache(v.id).fullName
        val newExpr = rewriteNames(v.expr)
        List(Ast.Val(v.id, newName, newExpr))
      case n: Ast.Namespace => flatten(n.children)
    }
  }

  def resolveNamespaces(env: MyEnvType, p: Program): FlatProgram = {
    implicit val namespace: String = ""
    val nextEnv = runAllTopLevel(env, p.stmts)
    p.stmts.foreach(s => nextEnv.get(s.name).foreach(_.value))
    val flatTopLevel = flatten(p.stmts)
    FlatProgram(flatTopLevel)
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
