package com.mistlang.lang

import com.mistlang.interpreter.RuntimeValue.{Lazy, Strict}
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Ast._
import com.mistlang.lang.ComptimeValue._
import com.mistlang.lang.TypeInterpreter.{TypeCache, makeTypeCache}
import com.mistlang.lang.Types._

class TypeInterpreter {
  import TypeInterpreter.error
  type MyEnvType = Env[RuntimeValue[TypedValue]]

  private def checkType(expected: Type, actual: Type): Unit = {
    (expected, actual) match {
      case (Types.AnyType, _)                                                                             =>
      case (e, a) if e == a                                                                               =>
      case (e: FuncType, a: FuncType) if (e.args == a.args) && (e.out == a.out) && (e.isStar == a.isStar) =>
      case _ => error(s"Type mismatch: expected ${expected}, got ${actual}")
    }
  }

  private def evalExpr(env: MyEnvType, expr: Ast.Expr, evalValues: Boolean)(implicit cache: TypeCache): TypedValue = {
    val res: TypedValue = expr match {
      case Ast.Literal(_, value) =>
        value match {
          case s: String  => TypedValue(StrType, Some(SimpleValue(s)))
          case i: Int     => TypedValue(IntType, Some(SimpleValue(i)))
          case b: Boolean => TypedValue(BoolType, Some(SimpleValue(b)))
        }
      case Ast.Ident(_, name) =>
        env
          .get(name)
          .getOrElse { error(s"$name not found") }
          .value
      case Ast.Call(_, func, args, _) =>
        val typedF = evalExpr(env, func, evalValues)
        typedF match {
          case TypedValue(FuncType(funcArgs, out, isStar), value, name, isComptime) =>
            val expectedArgs = if (isStar) {
              funcArgs.take(funcArgs.length - 1) ++ List.fill(args.length - funcArgs.length + 1)(funcArgs.last)
            } else funcArgs
            if (expectedArgs.length != args.length)
              error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${args.length}")

            val typedArgs = args.map(e => evalExpr(env, e, evalValues))

            expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
              checkType(expected, actual.tpe)
            }

            if (!isComptime) {
              val resValue = value match {
                case Some(f: Func) if evalValues && typedArgs.forall(_.value.isDefined) => Some(f.f(typedArgs))
                case _                                                                  => None
              }
              TypedValue(out, resValue.flatMap(_.value))
            } else {
              val f = value match {
                case Some(f: CachingFunc) => f
                case _                    => error("comptime funcs must be caching")
              }
              typedArgs.foreach { arg =>
                if (arg.value.isEmpty) error("Values to comptime functions must be known at compile time")
              }
              f.cache.get(typedArgs) match {
                case Some(value) => value._2
                case None =>
                  val cachedName = name.get + "$" + f.cache.size
                  val (newCache, unnamedRes) = f.f(typedArgs)
                  val res = unnamedRes.copy(name = Some(cachedName))
                  f.cache.put(typedArgs, (newCache, res))
                  res
              }
            }

          case TypedValue(TypeType, Some(resType @ StructType(expectedArgs)), _, _) =>
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

      case Ast.MemberRef(_, expr, memberName) =>
        val ref = evalExpr(env, expr, evalValues)

        ref match {
          case TypedValue(s: StructType, value: Option[Dict], _, _) =>
            val tpe = s.args.find(_._1 == memberName).getOrElse(error(s"Member name ${memberName} not found"))._2
            TypedValue(tpe, value.map(_.m(memberName)))
          case other => error(s"Cannot call member ref on $other")
        }
      case Ast.If(_, expr, success, fail) =>
        val cond = evalExpr(env, expr, evalValues)
        if (cond.tpe != BoolType) error("Condition must be boolean")

        val (evalSuccessValue, evalFailValue) = cond.value match {
          case Some(SimpleValue(condValue: Boolean)) if evalValues =>
            if (condValue) (true, false)
            else (false, true)
          case _ => (false, false)
        }

        val typedSucc = evalExpr(env, success, evalSuccessValue)
        val typedFail = evalExpr(env, fail, evalFailValue)

        val resType = if (typedSucc.tpe == typedFail.tpe) typedSucc.tpe else AnyType
        val value = cond.value.flatMap { c =>
          if (c.asInstanceOf[SimpleValue].value.asInstanceOf[Boolean]) typedSucc.value
          else typedFail.value
        }
        TypedValue(resType, value)
      case Ast.Block(_, stmts) => runAll(env.newScope, stmts, evalValues)._2
      case Ast.Lambda(_, name, args, outType, body, isComptime) =>
        val inputTypes = args.map { arg => evalExpr(env, arg.tpe, evalValues) }.map {
          case TypedValue(TypeType, Some(tpe), _, _) => tpe.asInstanceOf[Type]
          case other                                 => error(s"Cannot decode type of ${other}")
        }
        val expectedOut = outType.map(o => evalExpr(env, o, evalValues)).map {
          case TypedValue(TypeType, Some(tpe), _, _) => tpe.asInstanceOf[Type]
          case other                                 => error(s"Cannot decode type of ${other}")
        }

        val newEnv = args.zip(inputTypes).foldLeft(env.newScope) { case (curEnv, nextArg) =>
          curEnv.put(nextArg._1.name, Strict(TypedValue(nextArg._2, None)))
        }

        val withRec = (name, expectedOut) match {
          case (Some(name), Some(value)) =>
            newEnv.put(name, Strict(TypedValue(FuncType(inputTypes, value), None)))
          case _ => newEnv
        }

        val actualOut = evalExpr(withRec, body, evalValues)
        expectedOut.foreach { o => checkType(o, actualOut.tpe) }

        val resType = FuncType(inputTypes, expectedOut.getOrElse(actualOut.tpe))
        val resValue = if (!isComptime) {
          Func((values: List[TypedValue]) => {
            val newEnv = args.zip(values).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
              curEnv.put(arg.name, Strict(value))
            }
            evalExpr(newEnv, body, evalValues)
          })
        } else {
          CachingFunc((values: List[TypedValue]) => {
            val newEnv = args.zip(values).foldLeft(env.newScope) { case (curEnv, (arg, value)) =>
              curEnv.put(arg.name, Strict(value))
            }
            val newCache = makeTypeCache()
            (newCache, evalExpr(newEnv, body, evalValues)(newCache))
          })
        }
        TypedValue(resType, Some(resValue), name, isComptime)

      case s: Ast.Struct => evalStruct(env, s)
    }
    cache.put(expr.id, res)

    res
  }

  private def evalStruct(env: MyEnvType, s: Ast.Struct)(implicit cache: TypeCache): TypedValue = {
    val argTypes = s.args.map { a =>
      val aTyped = evalExpr(env, a.tpe, evalValues = true)
      aTyped match {
        case TypedValue(TypeType, Some(t: Type), _, _) => t
        case TypedValue(TypeType, None, _, _)          => AnyType
        case other                                     => error(s"cannot decode type $other")
      }
    }
    val resType = StructType(s.args.map(_.name).zip(argTypes))

    TypedValue(TypeType, Some(resType), None, isComptime = true)
  }

  val unit: TypedValue = TypedValue(UnitType, Some(UnitValue))

  private def run(env: MyEnvType, stmt: FnBodyStmt, evalValues: Boolean)(implicit
      cache: TypeCache
  ): (MyEnvType, TypedValue) = {
    stmt match {
      case expr: Expr => (env, evalExpr(env, expr, evalValues))
      case Ast.Val(_, name, expr) =>
        val evaluated = evalExpr(env, expr, evalValues)
        (env.put(name, Strict(evaluated)), unit)
    }
  }

  private def runAll(env: MyEnvType, stmts: List[FnBodyStmt], evalValues: Boolean)(implicit
      cache: TypeCache
  ): (MyEnvType, TypedValue) = {
    stmts.foldLeft((env, unit)) { case ((curEnv, _), nextStmt) =>
      run(curEnv, nextStmt, evalValues)
    }
  }

  private def runTopLevel(env: MyEnvType, stmt: FlatTopLevelStmt)(implicit cache: TypeCache): Unit = {
    stmt match {
      case d: Def => env.set(d.name, Lazy(() => evalExpr(env, d.lambda, evalValues = true)))
      case v: Val => env.set(v.name, Lazy(() => evalExpr(env, v.expr, evalValues = true)))
    }
  }

  private def runAllTopLevel(env: MyEnvType, stmts: List[FlatTopLevelStmt])(implicit cache: TypeCache): MyEnvType = {
    val newEnv = stmts.map(_.name).foldLeft(env) { case (curEnv, nextName) => curEnv.put(nextName, Strict(null)) }
    stmts.foreach(stmt => runTopLevel(newEnv, stmt))
    stmts.foreach(s => newEnv.get(s.name).foreach(_.value))
    newEnv
  }

  def runProgram(env: MyEnvType, p: Ast.FlatProgram)(implicit cache: TypeCache): TypedValue = {
    val nextEnv = runAllTopLevel(env, p.topLevelStmts)
    runAll(nextEnv, p.stmts, evalValues = true)._2
  }

}

object TypeInterpreter {

  type TypeCache = collection.mutable.Map[Int, TypedValue]
  def makeTypeCache() = collection.mutable.Map[Int, TypedValue]()
  case class TypeError(msg: String) extends RuntimeException(msg)
  def error(s: String) = throw TypeError(s)

  object StdEnv {
    val unitType = TypedValue(TypeType, Some(UnitType))
    val anyType = TypedValue(TypeType, Some(AnyType))
    val intType = TypedValue(TypeType, Some(IntType))
    val boolType = TypedValue(TypeType, Some(BoolType))
    val stringType = TypedValue(TypeType, Some(StrType))
    val typeType = TypedValue(TypeType, Some(TypeType))
  }

  private val stdEnv = {
    def f2Int(name: String, outType: Type, f: (Int, Int) => Any): TypedValue = {
      val func = Func {
        case TypedValue(IntType, Some(SimpleValue(a: Int)), _, _) ::
            TypedValue(IntType, Some(SimpleValue(b: Int)), _, _) :: Nil =>
          TypedValue(outType, Some(SimpleValue(f(a, b))))
      }
      val tpe = FuncType(List(IntType, IntType), outType)
      TypedValue(tpe, Some(func), Some(name))
    }

    def mkFunc(name: String, argTypes: List[Type], outType: Type, f: List[ComptimeValue] => ComptimeValue) = {
      val tpe = FuncType(argTypes, outType)
      val func = Func(args => TypedValue(outType, Some(f(args.map(_.value.get)))))
      TypedValue(tpe, Some(func), Some(name))
    }

    val basicTypes: Map[String, TypedValue] = Map(
      "plusOp" -> f2Int("plusOp", IntType, (a, b) => a + b),
      "minusOp" -> f2Int("minusOp", IntType, (a, b) => a - b),
      "productOp" -> f2Int("productOp", IntType, (a, b) => a * b),
      "smallerOp" -> f2Int("smallerOp", BoolType, (a, b) => a < b),
      "eqIntOp" -> TypedValue(
        FuncType(List(AnyType, AnyType), BoolType),
        Some(Func(args => TypedValue(BoolType, Some(SimpleValue(args(0) == args(1))))))
      ),
      "Func" -> TypedValue(
        FuncType(List(TypeType), TypeType, isStar = true),
        Some(Func(args => {
          val tpes = args.map {
            case TypedValue(TypeType, Some(tpe: Type), _, _) => tpe
            case other                                       => error(s"Could not decode type from $other")
          }
          TypedValue(TypeType, Some(FuncType(tpes.take(tpes.length - 1), tpes.last)))
        }))
      ),
      "intArrayMake" -> mkFunc(
        "intArrayMake",
        List(IntType),
        ArrayType(IntType),
        { case SimpleValue(a: Int) :: Nil => SimpleValue(new Array[Int](a)) }
      ),
      "intArrayGet" -> mkFunc(
        "intArrayGet",
        List(ArrayType(IntType), IntType),
        IntType,
        { case SimpleValue(arr: Array[Int]) :: SimpleValue(i: Int) :: Nil => SimpleValue(arr(i)) }
      ),
      "intArraySet" -> mkFunc(
        "intArraySet",
        List(ArrayType(IntType), IntType, IntType),
        UnitType,
        { case SimpleValue(arr: Array[Int]) :: SimpleValue(i: Int) :: SimpleValue(value: Int) :: Nil =>
          arr(i) = value
          UnitValue
        }
      ),
      "intArrayPrint" -> mkFunc(
        "intArrayPrint",
        List(ArrayType(IntType)),
        StrType,
        { case SimpleValue(arr: Array[Int]) :: Nil => SimpleValue(arr.toList.toString()) }
      ),
      "boolArrayMake" -> mkFunc(
        "boolArrayMake",
        List(IntType),
        ArrayType(BoolType),
        { case SimpleValue(a: Int) :: Nil => SimpleValue(new Array[Boolean](a)) }
      ),
      "boolArrayGet" -> mkFunc(
        "boolArrayGet",
        List(ArrayType(BoolType), IntType),
        BoolType,
        { case SimpleValue(arr: Array[Boolean]) :: SimpleValue(i: Int) :: Nil => SimpleValue(arr(i)) }
      ),
      "boolArraySet" -> mkFunc(
        "boolArraySet",
        List(ArrayType(BoolType), IntType, BoolType),
        UnitType,
        { case SimpleValue(arr: Array[Boolean]) :: SimpleValue(i: Int) :: SimpleValue(value: Boolean) :: Nil =>
          arr(i) = value
          UnitValue
        }
      ),
      "boolArrayPrint" -> mkFunc(
        "boolArrayPrint",
        List(ArrayType(BoolType)),
        StrType,
        { case SimpleValue(arr: Array[Boolean]) :: Nil => SimpleValue(arr.toList.toString()) }
      ),
      "Unit" -> StdEnv.unitType,
      "Any" -> StdEnv.anyType,
      "Int" -> StdEnv.intType,
      "Bool" -> StdEnv.boolType,
      "String" -> StdEnv.stringType,
      "Type" -> StdEnv.typeType,
      "True" -> TypedValue(BoolType, Some(SimpleValue(true))),
      "False" -> TypedValue(BoolType, Some(SimpleValue(false)))
    )

    Env.make[RuntimeValue[TypedValue]](
      basicTypes.map { case (name, v) => name -> Strict(v) },
      None
    )
  }

  def typeAll(p: FlatProgram): TypeCache = {
    val interpreter = new TypeInterpreter
    implicit val cache = makeTypeCache()
    interpreter.runProgram(stdEnv, p)
    cache
  }

  def typeStmts(p: FlatProgram): TypedValue = {
    implicit val cache = makeTypeCache()
    new TypeInterpreter().runProgram(stdEnv, p)
  }

}
