package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._
import com.mistlang.lang.ComptimeValue._
import com.mistlang.lang.Types._

class TypeInterpreter {
  import TypeInterpreter.error
  type MyEnvType = Env[RuntimeValue[TypedValue]]

  val cache = collection.mutable.Map[Int, TypedValue]()

  private def checkType(expected: Type, actual: Type): Unit = {
    (expected, actual) match {
      case (Types.AnyType, _)                                                                             =>
      case (e, a) if e == a                                                                               =>
      case (e: FuncType, a: FuncType) if (e.args == a.args) && (e.out == a.out) && (e.isStar == a.isStar) =>
      case _ => error(s"Type mismatch: expected ${expected}, got ${actual}")
    }
  }

  def evalExpr(env: MyEnvType, expr: Ast.Expr, evalValues: Boolean): TypedValue = {
    val res: TypedValue = expr match {
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
        val typedF = evalExpr(env, func, evalValues)
        typedF match {
          case TypedValue(FuncType(funcArgs, out, _, isStar), value) =>
            val expectedArgs = if (isStar) {
              funcArgs.take(funcArgs.length - 1) ++ List.fill(args.length - funcArgs.length + 1)(funcArgs.last)
            } else funcArgs
            if (expectedArgs.length != args.length)
              error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${args.length}")

            val typedArgs = args.map(e => evalExpr(env, e, evalValues))

            expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
              checkType(expected, actual.tpe)
            }

            val resValue = value match {
              case Some(f: Func) if evalValues && typedArgs.forall(_.value.isDefined) => Some(f.f(typedArgs))
              case _                                                                  => None
            }
            TypedValue(out, resValue.flatMap(_.value))
          case TypedValue(TypeType, Some(resType @ StructType(_, _, expectedArgs))) =>
            if (expectedArgs.length != args.length)
              error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${args.length}")

            val typedArgs = args.map(e => evalExpr(env, e, evalValues))
            expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
              checkType(expected._2, actual.tpe)
            }
            val resValue = if (evalValues && typedArgs.forall(_.value.isDefined)) {
              val map = expectedArgs.map(_._1).zip(typedArgs.map(_.value.get)).toMap
              Some(Dict(map))
            } else None
            TypedValue(resType, resValue)
          case _ => error(s"Cannot call $typedF")
        }
      case Ast.MethodRef(_, expr, methodName) =>
        val typedObj = evalExpr(env, expr, evalValues)
        typedObj.tpe match {
          case StructType(structName, namespace, _) =>
            val fullStructName = if (namespace.isEmpty) structName else namespace + "." + structName
            val fullMethodName = fullStructName + "$." + methodName
            val res = evalExpr(env, FastparseParser.parseExpr(fullMethodName, () => None), evalValues = false)
            res
          case other => error(s"Cannot call method of $other")
        }

      case Ast.MemberRef(_, expr, memberName) =>
        val ref = evalExpr(env, expr, evalValues)
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
        val cond = evalExpr(env, expr, evalValues)
        if (cond.tpe != BoolType) error("Condition must be boolean")

        val (evalSuccessValue, evalFailValue) = cond.value match {
          case Some(PrimitiveValue(condValue: Boolean)) if evalValues =>
            if (condValue) (true, false)
            else (false, true)
          case _ => (false, false)
        }

        val typedSucc = evalExpr(env, success, evalSuccessValue)
        val typedFail = evalExpr(env, fail, evalFailValue)

        val resType = if (typedSucc.tpe == typedFail.tpe) typedSucc.tpe else AnyType
        val value = cond.value.flatMap { c =>
          if (c.asInstanceOf[PrimitiveValue].value.asInstanceOf[Boolean]) typedSucc.value
          else typedFail.value
        }
        TypedValue(resType, value)
      case Ast.Block(_, stmts) => runAll(env.newScope, stmts, evalValues)._2
      case Ast.Lambda(_, name, args, outType, body) =>
        val funcName: String = name
          .map { n =>
            if (env.namespace.isEmpty) n
            else env.namespace + "." + n
          }
          .getOrElse("")

        val inputTypes = args.map { arg => evalExpr(env, arg.tpe, evalValues) }.map {
          case TypedValue(TypeType, Some(tpe)) => tpe.asInstanceOf[Type]
          case other                           => error(s"Cannot decode type of ${other}")
        }
        val expectedOut = outType.map(o => evalExpr(env, o, evalValues)).map {
          case TypedValue(TypeType, Some(tpe)) => tpe.asInstanceOf[Type]
          case other                           => error(s"Cannot decode type of ${other}")
        }

        val newEnv = args.zip(inputTypes).foldLeft(env.newScope) { case (curEnv, nextArg) =>
          curEnv.put(nextArg._1.name, Strict(TypedValue(nextArg._2, None)))
        }

        val withRec = (name, expectedOut) match {
          case (Some(name), Some(value)) =>
            newEnv.put(name, Strict(TypedValue(FuncType(inputTypes, value, funcName), None)))
          case _ => newEnv
        }

        val actualOut = evalExpr(withRec, body, evalValues)
        expectedOut.foreach { o => checkType(o, actualOut.tpe) }

        val resType = FuncType(inputTypes, expectedOut.getOrElse(actualOut.tpe), funcName)
        val resValue = Func((values: List[TypedValue]) => {
          val newEnv = args.zip(values).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
            curEnv.put(arg.name, Strict(value))
          }
          evalExpr(newEnv, body, evalValues)
        })
        TypedValue(resType, Some(resValue))

      case Ast.Comptime(id, expr) => ???
    }
    expr.id.foreach { id =>
      cache.put(id, res)
    }

    res
  }

  def evalStruct(env: MyEnvType, s: Ast.Struct): TypedValue = {
    val argTypes = s.args.map { a =>
      evalExpr(env, a.tpe, evalValues = false) match {
        case TypedValue(TypeType, Some(t: Type)) => t
        case other                               => error(s"cannot decode type $other")
      }
    }
    val resType = StructType(s.name, env.namespace, s.args.map(_.name).zip(argTypes))
    TypedValue(TypeType, Some(resType))
  }

  def evalNamespace(env: MyEnvType, n: Ast.Namespace): TypedValue = {
    val namespaceEnv = runAllTopLevel(env.namespaceScope(n.name), n.children)

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

  def unit: TypedValue = TypeInterpreter.StdEnv.unitType

  def forceTopLevelEval: Boolean = true

  def run(env: MyEnvType, stmt: Stmt, evalValues: Boolean): (MyEnvType, TypedValue) = {
    stmt match {
      case expr: Expr => (env, evalExpr(env, expr, evalValues))
      case Ast.Val(name, expr) =>
        val evaluated = evalExpr(env, expr, evalValues)
        (env.put(name, Strict(evaluated)), unit)
    }
  }

  def runAll(env: MyEnvType, stmts: List[Stmt], evalValues: Boolean): (MyEnvType, TypedValue) = {
    stmts.foldLeft((env, unit)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt, evalValues)
    }
  }

  protected def runTopLevel(env: MyEnvType, stmt: TopLevelStmt): Unit = {
    stmt match {
      case d: Def       => env.set(d.name, Lazy(() => evalExpr(env, d.lambda, evalValues = true)))
      case s: Struct    => env.set(s.name, Lazy(() => evalStruct(env, s)))
      case n: Namespace => env.set(n.name, Lazy(() => evalNamespace(env, n)))

    }
  }

  protected def runAllTopLevel(env: MyEnvType, stmts: List[TopLevelStmt]): MyEnvType = {
    val newEnv = stmts.map(_.name).foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }
    stmts.foreach(stmt => runTopLevel(newEnv, stmt))
    if (forceTopLevelEval) {
      stmts.foreach(s => newEnv.get(s.name).foreach(_.value))
    }
    newEnv
  }

  def runProgram(env: MyEnvType, p: Ast.Program): TypedValue = {
    val nextEnv = runAllTopLevel(env, p.topLevelStmts)
    runAll(nextEnv, p.stmts, evalValues = true)._2
  }

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
    def f2int(f: (Int, Int) => Int): Func = Func {
      case TypedValue(IntType, Some(PrimitiveValue(a: Int))) ::
          TypedValue(IntType, Some(PrimitiveValue(b: Int))) :: Nil =>
        TypedValue(IntType, Some(PrimitiveValue(f(a, b))))
    }

    val basicTypes: Map[String, TypedValue] = Map(
      "+" -> TypedValue(FuncType(List(IntType, IntType), IntType, "+"), Some(f2int((a, b) => a + b))),
      "-" -> TypedValue(FuncType(List(IntType, IntType), IntType, "-"), Some(f2int((a, b) => a - b))),
      "*" -> TypedValue(FuncType(List(IntType, IntType), IntType, "*"), Some(f2int((a, b) => a * b))),
      "==" -> TypedValue(
        FuncType(List(AnyType, AnyType), BoolType, "=="),
        Some(Func(args => TypedValue(BoolType, Some(PrimitiveValue(args(0) == args(1))))))
      ),
      "Func" -> TypedValue(
        FuncType(List(TypeType), TypeType, "Func", isStar = true),
        Some(Func(args => {
          val tpes = args.map {
            case TypedValue(TypeType, Some(tpe: Type)) => tpe
            case other                                 => error(s"Could not decode type from $other")
          }
          TypedValue(TypeType, Some(FuncType(tpes.take(tpes.length - 1), tpes.last, "")))
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

  def typeStmts(p: Program): TypedValue = new TypeInterpreter().runProgram(stdEnv, p)

}
