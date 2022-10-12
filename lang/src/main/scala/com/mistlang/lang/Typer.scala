package com.mistlang.lang

import com.mistlang.lang.Ast.Expr
import com.mistlang.lang.Type._
import com.mistlang.lang.TypedAst.{Synthetic, TypedExpr}

case class TypeError(msg: String) extends RuntimeException(msg)

object Typer {
  type TypeEnv = Env[ValueHolder[TypedAst]]

  def error(msg: String) = throw TypeError(msg)

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

  private def evalLambda(env: TypeEnv, f: Ast.FuncData, name: Option[String]): TypedAst.Lambda = {
    val isLambda = name.isEmpty

    val argTypes = f.args.map(ad => Type.Arg(ad.name, evalExp(env, ad.tpe).tpe))
    val argScope = argTypes.foldLeft(env.newScope) { case (curEnv, arg) =>
      curEnv.put(arg.name, Strict(Synthetic(arg.tpe)))
    }
    val outType = f.outType.map(evalExp(env, _))
    val fnScope = (name, outType) match {
      case (Some(n), Some(e)) => argScope.put(n, Strict(Synthetic(FuncType(argTypes, e.tpe, isLambda))))
      case _                  => argScope
    }
    val typedBody = evalExp(fnScope, f.body)
    outType.foreach { out =>
      checkArg("outType", out.tpe, typedBody.tpe)
    }

    TypedAst.Lambda(typedBody, FuncType(argTypes, typedBody.tpe, isLambda))
  }

  private def evalCall(env: TypeEnv, c: Ast.Call): TypedExpr = {
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

  private def evalIf(env: TypeEnv, i: Ast.If): TypedExpr = {
    val typedCond = evalExp(env, i.expr)
    typedCond.tpe match {
      case BoolType =>
        val success = evalExp(env, i.succ)
        val fail = evalExp(env, i.fail)
        TypedAst.If(typedCond, success, fail)
      case other => error(s"Expected boolean in condition - got $other")
    }
  }

  private def evalExp(env: TypeEnv, ast: Expr): TypedExpr = {
    ast match {
      case Ast.Literal(value) =>
        val tpe = value match {
          case _: Int     => IntType
          case _: Boolean => BoolType
          case _: String  => StrType
        }
        TypedAst.Literal(value, tpe)
      case Ast.Tuple(children) => TypedAst.Tuple(children.map(c => evalExp(env, c)))
      case l: Ast.Lambda       => evalLambda(env, l.data, l.name)
      case l: Ast.Ident =>
        val t = env.get(l.name).map(_.value).getOrElse {
          error(s"${l.name} not found")
        }
        TypedAst.Ident(l.name, t.tpe)
      case c: Ast.Call      => evalCall(env, c)
      case Ast.Block(stmts) => TypedAst.Block(eval(env.newScope, stmts))
      case i: Ast.If        => evalIf(env, i)
    }
  }

  def eval(env: TypeEnv, stmts: List[Ast]): List[TypedAst] = {
    var topLevelEnv = env
    stmts.foreach {
      case d: Ast.Def =>
        topLevelEnv = topLevelEnv.put(
          d.name,
          Lazy(
            Ast.Lambda(d.data, Some(d.name)),
            () => topLevelEnv,
            (env, ast: Ast.Expr) => evalExp(env, ast)
          )
        )
      case _ => ()
    }
    val defs = stmts.collect { case d: Ast.Def =>
      TypedAst.Def(d.name, topLevelEnv.get(d.name).get.value.asInstanceOf[TypedAst.Lambda])
    }
    val others = stmts
      .filter {
        case _: Ast.Def => false
        case _          => true
      }
      .foldLeft((topLevelEnv, Nil: List[TypedAst])) { case ((curEnv, curAsts), stmt) =>
        stmt match {
          case e: Expr => (curEnv, curAsts :+ evalExp(curEnv, e))
          case Ast.Val(name, expr) =>
            val t = evalExp(curEnv, expr)
            val newExpr = t.tpe match {
              case f: FuncType if !f.isLambda =>
                TypedAst.Lambda(
                  TypedAst.Call(t, f.args.map(a => TypedAst.Ident(a.name, a.tpe)), f.outType),
                  f.copy(isLambda = true)
                )
              case _ => t
            }
            (curEnv.put(name, Strict(Synthetic(newExpr.tpe))), curAsts :+ TypedAst.Val(name, newExpr))
        }
      }
    defs ::: others._2
  }
}

sealed trait TypedAst {
  def tpe: Type
}

object TypedAst {
  sealed trait TypedExpr extends TypedAst

  case class Literal(value: Any, tpe: Type) extends TypedExpr
  case class Ident(name: String, tpe: Type) extends TypedExpr
  case class Tuple(exprs: List[TypedAst]) extends TypedExpr {
    val tpe: Type = TupleType(exprs.map(_.tpe))
  }
  case class Block(stmts: List[TypedAst]) extends TypedExpr {
    override val tpe: Type = stmts.lastOption.map(_.tpe).getOrElse(UnitType)
  }
  case class Call(func: TypedExpr, args: List[TypedExpr], tpe: Type) extends TypedExpr
  case class Lambda(body: TypedExpr, tpe: FuncType) extends TypedExpr
  case class Val(name: String, expr: TypedExpr) extends TypedAst {
    override def tpe: Type = UnitType
  }
  case class Def(name: String, func: Lambda) extends TypedAst {
    override def tpe: Type = UnitType
  }
  case class If(expr: TypedExpr, success: TypedExpr, fail: TypedExpr) extends TypedExpr {
    val tpe: Type = {
      if (success.tpe == fail.tpe) success.tpe
      else AnyType
    }
  }
  case class Synthetic(tpe: Type) extends TypedExpr

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
