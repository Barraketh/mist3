package com.mistlang.lang

import com.mistlang.lang.Ast._
import com.mistlang.lang.RuntimeValue.{BoolVal, IntVal, StrVal}
import com.mistlang.lang.Type._
import com.mistlang.lang.TypedAst.{Lazy, Synthetic}

case class TypeError(msg: String) extends RuntimeException(msg)

object Typer {
  def error(msg: String) = throw TypeError(msg)

  def checkArg(name: String, expected: TaggedType, actual: TaggedType): Unit = {
    (expected.t, actual.t) match {
      case (AnyType, _)     => ()
      case (e, a) if e == a => ()
      case (t1: TupleType, t2: TupleType) =>
        if (t1.arr.length != t2.arr.length) error(s"Type mismatch for $name - expected $expected, got $actual")
        else t1.arr.zip(t2.arr).foreach { case (e, a) => checkArg(name, e, a) }
      case _ =>
        error(s"Type mismatch for $name - expected $expected, got $actual")
    }
  }

  private def evalLiteral(l: Literal): TypedAst.Expr = {
    val tpe = l.value match {
      case i: Int     => TaggedType(IntType, "value" -> IntVal(i))
      case b: Boolean => TaggedType(BoolType, "value" -> BoolVal(b))
      case s: String  => TaggedType(StrType, "value" -> StrVal(s))
    }
    TypedAst.Literal(l.value, tpe)
  }

  private def evalIdent(env: TypeEnv, i: Ident): TypedAst.Expr = {
    val t = env.typeEnv.get(i.name).getOrElse {
      error(s"${i.name} not found")
    }
    val isLazy = t match {
      case _: Lazy => true
      case _       => false
    }
    TypedAst.Ident(i.name, t.tpe, isLazy)
  }

  private def evalBlock(env: TypeEnv, b: Block): TypedAst.Expr = {
    val typedStmts = eval(env.newScope, b.stmts)
    TypedAst.Block(typedStmts, typedStmts.lastOption.map(_.tpe).getOrElse(TaggedType.unitType))
  }

  private def evalLambda(env: TypeEnv, f: Lambda): TypedAst.Expr = {
    val isLambda = f.name.isEmpty

    val argTypes = f.args.map(ad => Type.Arg(ad.name, Interpreter.evalExp(env.runEnv, ad.tpe).asInstanceOf[TaggedType]))
    val argScope = argTypes.foldLeft(env.newScope) { case (curEnv, arg) =>
      curEnv.put(arg.name, TypedAst.Synthetic(arg.tpe))
    }
    val outType = f.outType.map(e => Interpreter.evalExp(env.runEnv, e).asInstanceOf[TaggedType])
    val fnScope = (f.name, outType) match {
      case (Some(n), Some(e)) => argScope.put(n, TypedAst.Synthetic(TaggedType(BasicFuncType(argTypes, e, isLambda))))
      case _                  => argScope
    }
    val typedBody = evalExp(fnScope, f.body)
    outType.foreach { out =>
      checkArg("outType", out, typedBody.tpe)
    }

    TypedAst.Lambda(typedBody, TaggedType(BasicFuncType(argTypes, typedBody.tpe, isLambda)))
  }

  private def evalCall(env: TypeEnv, c: Call): TypedAst.Expr = {
    val fTyped = evalExp(env, c.func)
    val resolvedTypes = c.args.map(e => evalExp(env, e))

    fTyped.tpe.t match {
      case l: InlinedLambda =>
        val argTypes =
          l.ast.args.map(ad => Type.Arg(ad.name, Interpreter.evalExp(env.runEnv, ad.tpe).asInstanceOf[TaggedType]))
        val resolvedArgs = c.args.map(evalExp(env, _))

        argTypes.zip(resolvedArgs).foreach { case (arg, resolved) =>
          checkArg(arg.name, arg.tpe, resolved.tpe)
        }
        val newScope =
          argTypes.zip(resolvedArgs).foldLeft(env.newScope) { case (curEnv, arg) => curEnv.put(arg._1.name, arg._2) }
        val resolvedBody = evalExp(newScope, l.ast.body)
        l.ast.outType.foreach { e =>
          val outType = Interpreter.evalExp(env.runEnv, e).asInstanceOf[TaggedType]
          checkArg("outType", outType, resolvedBody.tpe)
        }
        TypedAst.Block(
          argTypes.zip(resolvedArgs).map { case (arg, exp) => TypedAst.Val(arg.name, exp, TaggedType.unitType) } :+
            resolvedBody,
          resolvedBody.tpe
        )
      case f: FuncType =>
        val outType = f.f(resolvedTypes.map(_.tpe))
        TypedAst.Call(fTyped, resolvedTypes, outType)
      case other => error(s"Cannot call $other")
    }
  }

  private def evalIf(env: TypeEnv, i: If): TypedAst.Expr = {
    val typedCond = evalExp(env, i.expr)
    typedCond.tpe.t match {
      case BoolType =>
        val success = evalExp(env, i.success)
        val fail = evalExp(env, i.fail)
        val baseType = if (success.tpe.t == fail.tpe.t) success.tpe.t else AnyType
        val tags: Map[String, RuntimeValue] =
          if (baseType != AnyType) success.tpe.tags.toSet.intersect(fail.tpe.tags.toSet).toMap else Map.empty
        TypedAst.If(typedCond, success, fail, TaggedType(baseType, tags))
      case other => error(s"Expected boolean in condition - got $other")
    }
  }

  private def evalVal(env: TypeEnv, v: Val): (TypeEnv, TypedAst) = {
    val resolved = evalExp(env, v.expr)
    val nextExpr = resolved.tpe.t match {
      case f: BasicFuncType if !f.isLambda =>
        TypedAst.Lambda(
          TypedAst.Call(resolved, f.args.map(a => TypedAst.Ident(a.name, a.tpe, false)), f.outType),
          TaggedType(f.copy(isLambda = true))
        )
      case _ => resolved
    }
    (env.put(v.name, nextExpr), TypedAst.Val(v.name, nextExpr, TaggedType.unitType))
  }

  def evalExp(env: TypeEnv, ast: Expr): TypedAst.Expr = {
    ast match {
      case l: Literal => evalLiteral(l)
      case i: Ident   => evalIdent(env, i)
      case b: Block   => evalBlock(env, b)
      case c: Call    => evalCall(env, c)
      case i: If      => evalIf(env, i)
      case l: Lambda =>
        if (l.isInline) Synthetic(TaggedType(InlinedLambda(l)))
        else evalLambda(env, l)
    }
  }

  def eval(env: TypeEnv, stmts: List[Ast]): List[TypedAst] = {
    val (defs, others) = stmts.partition {
      case _: Def => true
      case _      => false
    }
    val topLevel = defs.collect { case d: Def =>
      d.name -> ((curEnv: TypeEnv) => {
        val resolved = evalExp(curEnv, d.expr)
        d.expr match {
          case _: Ast.Lambda => resolved
          case _             => Lazy(resolved)
        }
      })
    }
    val topLevelEnv = env.putTopLevel(topLevel)

    (defs ::: others)
      .foldLeft(topLevelEnv -> (Nil: List[TypedAst])) { case ((curEnv, curAsts), stmt) =>
        stmt match {
          case e: Expr => (curEnv, curAsts :+ evalExp(curEnv, e))
          case d: Def =>
            val defExpr = curEnv.typeEnv.get(d.name).get
            defExpr match {
              case Synthetic(TaggedType(InlinedLambda(_), _)) => (curEnv, curAsts)
              case _ => (curEnv, curAsts :+ TypedAst.Def(d.name, defExpr, TaggedType.unitType))
            }
          case v: Val =>
            val (nextEnv, nextAst) = evalVal(curEnv, v)
            (nextEnv, curAsts :+ nextAst)
        }
      }
      ._2
  }
}

case class TypeEnv(typeEnv: Env[TypedAst.Expr], runEnv: Env[RuntimeValue]) {
  def newScope: TypeEnv = TypeEnv(typeEnv.newScope, runEnv.newScope)

  def put(name: String, expr: TypedAst.Expr): TypeEnv = copy(typeEnv = typeEnv.put(name, expr))

  def putTopLevel(toPut: Iterable[(String, TypeEnv => TypedAst.Expr)]): TypeEnv = {
    // TODO: Make this work with runtime values
    copy(typeEnv = typeEnv.putTopLevel(toPut.map { case (name, eval) =>
      name -> ((curEnv: Env[TypedAst.Expr]) => eval(TypeEnv(curEnv, runEnv)))
    }))
  }
}

object TypeEnv {
  val typeEnv = TyperIntrinsics.intrinsics.foldLeft(Env.empty[TypedAst.Expr]) { case (curEnv, (name, f)) =>
    curEnv.put(name, Synthetic(f))
  }
  val runtimeEnv = TyperIntrinsics.runtimeIntrinsics.foldLeft(Env.empty[RuntimeValue]) { case (curEnv, (name, v)) =>
    curEnv.put(name, v)
  }
  val env = TypeEnv(typeEnv, runtimeEnv)
}
