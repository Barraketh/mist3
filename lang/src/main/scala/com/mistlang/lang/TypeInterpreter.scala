package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.Strict
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast.Program
import com.mistlang.lang.ComptimeValue._
import com.mistlang.lang.Types._

class TypeInterpreter extends BaseInterpreter[TypedValue] {
  import TypeInterpreter.error

  val cache = collection.mutable.Map[Int, TypedValue]()

  private def checkType(expected: Type, actual: Type): Unit = {
    if (!(expected == Types.AnyType || expected == actual)) {
      error(s"Type mismatch: expected ${expected}, got ${actual}")
    }
  }

  override def evalExpr(env: MyEnvType, expr: Ast.Expr): TypedValue = {
    val res = expr match {
      case Ast.Literal(_, value) =>
        value match {
          case s: String  => TypedValue(StrType, Some(PrimitiveValue(s)))
          case i: Int     => TypedValue(IntType, Some(PrimitiveValue(i)))
          case b: Boolean => TypedValue(BoolType, Some(PrimitiveValue(b)))
        }
      case Ast.Ident(_, name) =>
        env
          .get(name)
          .getOrElse { error(s"$name not found") }
          .value
      case Ast.Call(_, func, args, _) =>
        val typedF = evalExpr(env, func)
        typedF match {
          case TypedValue(FuncType(funcArgs, out, isStar), value) =>
            val expectedArgs = if (isStar) {
              funcArgs.take(funcArgs.length - 1) ++ List.fill(args.length - funcArgs.length + 1)(funcArgs.last)
            } else funcArgs
            if (expectedArgs.length != args.length)
              error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${args.length}")

            val typedArgs = args.map(e => evalExpr(env, e))

            expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
              checkType(expected, actual.tpe)
            }

            val resValue = if (value.isDefined && typedArgs.forall(_.value.isDefined)) {
              Some(value.get.asInstanceOf[Func].f(typedArgs))
            } else None
            TypedValue(out, resValue.flatMap(_.value))
          case TypedValue(TypeType, Some(resType @ StructType(_, _, expectedArgs))) =>
            if (expectedArgs.length != args.length)
              error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${args.length}")

            val typedArgs = args.map(e => evalExpr(env, e))
            expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
              checkType(expected._2, actual.tpe)
            }
            val resValue = if (typedArgs.forall(_.value.isDefined)) {
              val map = expectedArgs.map(_._1).zip(typedArgs.map(_.value.get)).toMap
              Some(Dict(map))
            } else None
            TypedValue(resType, resValue)
          case _ => error(s"Cannot call $typedF")
        }
      case Ast.MemberRef(_, expr, memberName) =>
        val ref = evalExpr(env, expr)
        val resType = ref.tpe match {
          case s: StructType =>
            s.args.find(_._1 == memberName).getOrElse(error(s"Member name ${memberName} not found"))._2
          case n: NamespaceType =>
            n.children.find(_._1 == memberName).getOrElse(error(s"Member name ${memberName} not found"))._2
          case other =>
            throw error(s"Can not get member of ${other}")
        }
        val value = ref.value.map { v =>
          v.asInstanceOf[Dict].m(memberName)
        }
        TypedValue(resType, value)
      case Ast.If(_, expr, success, fail) =>
        val cond = evalExpr(env, expr)
        if (cond.tpe != BoolType) error("Condition must be boolean")

        val typedSucc = evalExpr(env, success)
        val typedFail = evalExpr(env, fail)

        val resType = if (typedSucc.tpe == typedFail.tpe) typedSucc.tpe else AnyType
        val value = cond.value.flatMap { c =>
          if (c.asInstanceOf[PrimitiveValue].value.asInstanceOf[Boolean]) typedSucc.value
          else typedFail.value
        }
        TypedValue(resType, value)
      case Ast.Block(_, stmts) => runAll(env.newScope, stmts)._2
      case Ast.Lambda(_, name, args, outType, body) =>
        val inputTypes = args.map { arg => evalExpr(env, arg.tpe) }.map {
          case TypedValue(TypeType, Some(tpe)) => tpe.asInstanceOf[Type]
          case other                           => error(s"Cannot decode type of ${other}")
        }
        val expectedOut = outType.map(o => evalExpr(env, o)).map {
          case TypedValue(TypeType, Some(tpe)) => tpe.asInstanceOf[Type]
          case other                           => error(s"Cannot decode type of ${other}")
        }

        val newEnv = args.zip(inputTypes).foldLeft(env.newScope) { case (curEnv, nextArg) =>
          curEnv.put(nextArg._1.name, Strict(TypedValue(nextArg._2, None)))
        }

        val withRec = (name, expectedOut) match {
          case (Some(name), Some(value)) => newEnv.put(name, Strict(TypedValue(FuncType(inputTypes, value), None)))
          case _                         => newEnv
        }

        val actualOut = evalExpr(withRec, body)
        expectedOut.foreach { o => checkType(o, actualOut.tpe) }

        val resType = FuncType(inputTypes, expectedOut.getOrElse(actualOut.tpe))
        val resValue = Func((values: List[TypedValue]) => {
          val newEnv = args.zip(values).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
            curEnv.put(arg.name, Strict(value))
          }
          evalExpr(newEnv, body)
        })
        TypedValue(resType, Some(resValue))

      case Ast.Comptime(id, expr) => ???
    }
    cache.put(expr.id, res)
    res
  }

  override def evalStruct(env: MyEnvType, s: Ast.Struct, path: String): TypedValue = {
    val argTypes = s.args.map { a =>
      evalExpr(env, a.tpe) match {
        case TypedValue(TypeType, Some(t: Type)) => t
        case other                               => error(s"cannot decode type $other")
      }
    }
    val resType = StructType(s.name, path, s.args.map(_.name).zip(argTypes))
    TypedValue(TypeType, Some(resType))
  }

  override def evalNamespace(env: MyEnvType, n: Ast.Namespace, path: String): TypedValue = {
    val newPath = if (path.isEmpty) n.name else path + "." + n.name
    val namespaceEnv = runAllTopLevel(env.newScope, n.children, newPath)

    val names = n.children.map(_.name)
    val resType = NamespaceType(names.map(name => name -> namespaceEnv.get(name).get.value.tpe).toMap)
    val value = Dict(
      names
        .map(name => name -> namespaceEnv.get(name).get.value.value)
        .collect { case (name, Some(value)) =>
          name -> value
        }
        .toMap
    )
    TypedValue(resType, Some(value))
  }

  override def unit: TypedValue = TypeInterpreter.StdEnv.unitType

  override def forceTopLevelEval: Boolean = true

}

object TypeInterpreter {

  type TypeCache = collection.mutable.Map[Int, TypedValue]
  case class TypeError(msg: String) extends RuntimeException(msg)
  def error(s: String) = throw TypeError(s)

  object StdEnv {
    val unitType = TypedValue(TypeType, Some(UnitType))
    val anyType = TypedValue(TypeType, Some(AnyType))
    val intType = TypedValue(TypeType, Some(IntType))
    val stringType = TypedValue(TypeType, Some(StrType))
  }

  private val stdEnv = {
    val basicTypes: Map[String, TypedValue] = Map(
      "+" -> TypedValue(FuncType(List(IntType, IntType), IntType), None),
      "-" -> TypedValue(FuncType(List(IntType, IntType), IntType), None),
      "*" -> TypedValue(FuncType(List(IntType, IntType), IntType), None),
      "==" -> TypedValue(FuncType(List(AnyType, AnyType), BoolType), None),
      "Func" -> TypedValue(
        FuncType(List(TypeType), TypeType, isStar = true),
        Some(Func(args => {
          val tpes = args.map {
            case TypedValue(TypeType, Some(tpe: Type)) => tpe
            case other                                 => error(s"Could not decode type from $other")
          }
          TypedValue(TypeType, Some(FuncType(tpes.take(tpes.length - 1), tpes.last)))
        }))
      ),
      "Unit" -> StdEnv.unitType,
      "Any" -> StdEnv.anyType,
      "Int" -> StdEnv.intType,
      "String" -> StdEnv.stringType
    ).map { case (key, value) =>
      key -> value
    }
    Env.make[RuntimeValue[TypedValue]](
      basicTypes.map { case (name, v) => name -> Strict(v) },
      None
    )
  }

  def typeAll(p: Program): TypeCache = {
    val interpreter = new TypeInterpreter
    interpreter.runProgram(stdEnv, p)
    interpreter.cache
  }

  def typeStmts(p: Program): Type = new TypeInterpreter().runProgram(stdEnv, p).tpe

}
