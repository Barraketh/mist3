package com.mistlang.lang

import com.mistlang.lang.Ast._
import com.mistlang.lang.Type._
import com.mistlang.lang.TypedAst.Synthetic

case class TypeError(msg: String) extends RuntimeException(msg)

object Typer {
  type TypeEnv = Env[TypedAst.Expr]

  def error(msg: String) = throw TypeError(msg)

  def evalLiteral(l: Literal): TypedAst.Expr = {
    val tpe = l.value match {
      case _: Int     => IntType
      case _: Boolean => BoolType
      case _: String  => StrType
    }
    TypedAst.Literal(l.value, tpe)
  }

  def evalIdent(env: Typer.TypeEnv, i: Ident): TypedAst.Expr = {
    val t = env.get(i.name).getOrElse {
      error(s"${i.name} not found")
    }
    TypedAst.Ident(i.name, t.tpe)
  }

  def evalTuple(env: Typer.TypeEnv, t: Tuple): TypedAst.Expr = {
    val typedExprs = t.exprs.map(c => evalExp(env, c))
    TypedAst.Tuple(typedExprs, TupleType(typedExprs.map(_.tpe)))
  }

  def evalBlock(env: Typer.TypeEnv, b: Block): TypedAst.Expr = {
    val typedStmts = eval(env.newScope, b.stmts)
    TypedAst.Block(typedStmts, typedStmts.lastOption.map(_.tpe).getOrElse(UnitType))
  }

  private def checkArg(name: String, expected: Type, actual: Type): Unit = {
    (expected, actual) match {
      case (AnyType, _)     => ()
      case (e, a) if e == a => ()
      case (t1: TupleType, t2: TupleType) =>
        if (t1.arr.length != t2.arr.length) error(s"Type mismatch for $name - expected $expected, got $actual")
        else t1.arr.zip(t2.arr).foreach { case (e, a) => checkArg(name, e, a) }
      case _ => error(s"Type mismatch for $name - expected $expected, got $actual")
    }
  }

  private def evalLambda(env: TypeEnv, f: Lambda): TypedAst.Expr = {
    val isLambda = f.name.isEmpty

    val argTypes = f.args.map(ad => Type.Arg(ad.name, evalExp(env, ad.tpe).tpe))
    val argScope = argTypes.foldLeft(env.newScope) { case (curEnv, arg) =>
      curEnv.put(arg.name, TypedAst.Synthetic(arg.tpe))
    }
    val outType = f.outType.map(evalExp(env, _))
    val fnScope = (f.name, outType) match {
      case (Some(n), Some(e)) => argScope.put(n, TypedAst.Synthetic(FuncType(argTypes, e.tpe, isLambda)))
      case _                  => argScope
    }
    val typedBody = evalExp(fnScope, f.body)
    outType.foreach { out =>
      checkArg("outType", out.tpe, typedBody.tpe)
    }

    TypedAst.Lambda(typedBody, FuncType(argTypes, typedBody.tpe, isLambda))
  }

  private def evalCall(env: TypeEnv, c: Call): TypedAst.Expr = {
    val fTyped = evalExp(env, c.func)
    fTyped.tpe match {
      case f: FuncType =>
        if (f.args.length != c.args.length)
          error(s"Unexpected number of args - expected ${f.args.length}, got ${c.args.length}")

        val resolvedTypes = c.args.map(e => evalExp(env, e))
        f.args.zip(resolvedTypes).foreach { case (arg, actual) =>
          checkArg(arg.name, arg.tpe, actual.tpe)
        }
        TypedAst.Call(fTyped, resolvedTypes, f.outType)
      case other => error(s"Cannot call $other")
    }
  }

  private def evalIf(env: TypeEnv, i: If): TypedAst.Expr = {
    val typedCond = evalExp(env, i.expr)
    typedCond.tpe match {
      case BoolType =>
        val success = evalExp(env, i.success)
        val fail = evalExp(env, i.fail)
        val tpe: Type = {
          if (success.tpe == fail.tpe) success.tpe
          else AnyType
        }
        TypedAst.If(typedCond, success, fail, tpe)
      case other => error(s"Expected boolean in condition - got $other")
    }
  }

  private def evalVal(env: TypeEnv, v: Val): (TypeEnv, TypedAst) = {
    val resolved = evalExp(env, v.expr)
    val nextExpr = resolved.tpe match {
      case f: FuncType if !f.isLambda =>
        TypedAst.Lambda(
          TypedAst.Call(resolved, f.args.map(a => TypedAst.Ident(a.name, a.tpe)), f.outType),
          f.copy(isLambda = true)
        )
      case _ => resolved
    }
    (env.put(v.name, nextExpr), TypedAst.Val(v.name, nextExpr, UnitType))
  }

  private def evalDef(env: TypeEnv, d: Def): TypedAst =
    TypedAst.Def(d.name, env.get(d.name).get.asInstanceOf[TypedAst.Lambda], UnitType)

  def evalExp(env: TypeEnv, ast: Expr): TypedAst.Expr = {
    ast match {
      case l: Literal => evalLiteral(l)
      case i: Ident   => evalIdent(env, i)
      case t: Tuple   => evalTuple(env, t)
      case b: Block   => evalBlock(env, b)
      case c: Call    => evalCall(env, c)
      case i: If      => evalIf(env, i)
      case l: Lambda  => evalLambda(env, l)
    }
  }

  def eval(env: TypeEnv, stmts: List[Ast]): List[TypedAst] = {
    val (defs, others) = stmts.partition {
      case _: Def => true
      case _      => false
    }
    val topLevel = defs.collect { case d: Def =>
      d.name -> ((curEnv: TypeEnv) => evalExp(curEnv, d.func))
    }
    val topLevelEnv = env.putTopLevel(topLevel)

    (defs ::: others)
      .foldLeft(topLevelEnv -> (Nil: List[TypedAst])) { case ((curEnv, curAsts), stmt) =>
        stmt match {
          case e: Expr => (curEnv, curAsts :+ evalExp(curEnv, e))
          case d: Def  => (curEnv, curAsts :+ evalDef(curEnv, d))
          case v: Val =>
            val (nextEnv, nextAst) = evalVal(curEnv, v)
            (nextEnv, curAsts :+ nextAst)
        }
      }
      ._2
  }
}

object TyperEnv {
  val env = TyperIntrinsics.intrinsics.foldLeft(Env.empty[TypedAst.Expr]) { case (curEnv, (name, f)) =>
    curEnv.put(name, Synthetic(f))
  }
}

sealed trait Type

object Type {
  object IntType extends Type
  object StrType extends Type
  object BoolType extends Type
  object UnitType extends Type
  object AnyType extends Type

  case class TupleType(arr: List[Type]) extends Type

  case class Arg(name: String, tpe: Type)
  case class FuncType(args: List[Arg], outType: Type, isLambda: Boolean) extends Type

}
