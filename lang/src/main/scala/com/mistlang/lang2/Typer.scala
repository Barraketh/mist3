package com.mistlang.lang2

import com.mistlang.interpreter.RuntimeValue.Strict
import com.mistlang.interpreter.{Env, RuntimeValue}
import com.mistlang.lang.Types._
import com.mistlang.lang.{Type, TypeError, Types}
import com.mistlang.lang2.Ast._

object Typer {
  type TypeCache = collection.mutable.Map[Int, TypeObject]

  case class TypeObject(tpe: Type, tags: Map[String, Any] = Map.empty) {
    def stringTag(name: String): String = tags.get(name) match {
      case Some(s: String) => s
      case Some(other)     => error(s"Expected String tag for $this.$name - got $other")
      case None            => error(s"Expected $this.$name to be statically known")
    }
  }

  def error(s: String) = throw TypeError(s)

  private def checkType(expected: Type, actual: Type): Unit = {
    if (!(expected == Types.AnyType || expected == actual)) {
      error(s"Type mismatch: expected ${expected}, got ${actual}")
    }
  }

  class TyperVisitor extends Visitor[TypeObject] {
    val map: TypeCache = collection.mutable.Map.empty
    override def unit: TypeObject = TypeObject(UnitType)
    override def asT(a: Any): TypeObject = a match {
      case t: TypeObject               => t
      case f: Function[List[Any], Any] => TypeObject(TypeConstructor(l => f(l).asInstanceOf[TypeObject]))
      case other                       => error(s"$other is not a type")
    }
    override def literal(l: Literal): TypeObject = {
      val tpe = l.value match {
        case _: Boolean => BoolType
        case _: Int     => IntType
        case _: String  => StrType
        case null       => NullType
      }
      TypeObject(tpe, Map("value" -> l.value))
    }
    override def call(c: Call, env: Env[RuntimeValue]): TypeObject = {
      val typedF = evalExpr(c.func, env)
      typedF.tpe match {
        case FuncType(expectedArgs, out) =>
          if (expectedArgs.length != c.args.length)
            error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${c.args.length}")

          val typedArgs = c.args.map(e => evalExpr(e, env))
          expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
            checkType(expected, actual.tpe)
          }
          TypeObject(out)
        case TypeConstructor(f) =>
          val typedArgs = c.args.map(e => evalExpr(e, env))
          f(typedArgs)
        case StructType(_, _, expectedArgs) =>
          if (expectedArgs.length != c.args.length)
            error(s"Wrong number of arguments: expected ${expectedArgs.length}, got ${c.args.length}")

          val typedArgs = c.args.map(e => evalExpr(e, env))
          expectedArgs.zip(typedArgs).foreach { case (expected, actual) =>
            checkType(expected._2, actual.tpe)
          }
          typedF
        case _ => error(s"Cannot call ${typedF}")
      }
    }
    override def lambda(l: Lambda, env: Env[RuntimeValue]): TypeObject = {
      val inputTypes = l.args.map { arg =>
        asT(Interpreter.evalExpr(arg.tpe, env))
      }
      val expectedOut = l.outType.map(o => asT(Interpreter.evalExpr(o, env)))

      val newEnv = l.args.zip(inputTypes).foldLeft(env.newScope) { case (curEnv, nextArg) =>
        curEnv.put(nextArg._1.name, Strict(nextArg._2))
      }

      val withRec = (l.name, expectedOut) match {
        case (Some(name), Some(value)) =>
          newEnv.put(name, Strict(TypeObject(FuncType(inputTypes.map(_.tpe), value.tpe))))
        case _ => newEnv
      }

      val actualOut = evalExpr(l.body, withRec)
      expectedOut.foreach { o =>
        checkType(o.tpe, actualOut.tpe)
      }

      TypeObject(FuncType(inputTypes.map(_.tpe), expectedOut.getOrElse(actualOut).tpe))
    }

    override def as(a: As, env: Env[RuntimeValue]): TypeObject = asT(Interpreter.evalExpr(a.tpeExpr, env))
    override def cache(id: Int, t: TypeObject): Unit = map.put(id, t)
  }

  private val stdEnv = {
    val basicTypes: Map[String, TypeObject] = Map(
      "+" -> (FuncType(List(IntType, IntType), IntType)),
      "-" -> (FuncType(List(IntType, IntType), IntType)),
      "*" -> (FuncType(List(IntType, IntType), IntType)),
      "==" -> (FuncType(List(AnyType, AnyType), BoolType)),
      "Unit" -> (UnitType),
      "Any" -> (AnyType),
      "Int" -> (IntType),
      "String" -> (StrType),
      "if" -> TypeConstructor.make { case BoolType :: FuncType(_, succ) :: FuncType(_, fail) :: Nil =>
        if (succ == fail) succ else AnyType
      }
    ).map { case (key, value) =>
      key -> TypeObject(value)
    }

    val typeConstructors: Map[String, Any] = Map(
      "StructType" -> RuntimeValue.Func({ case (name: String) :: args =>
        val obj = args
          .grouped(2)
          .map { case (key: String) :: (tpe: TypeObject) :: Nil =>
            key -> tpe.tpe
          }
          .toList
        TypeObject(StructType(name, "", obj))
      }),
      "Object" -> RuntimeValue.Func({ case args: List[TypeObject] =>
        val obj = args
          .grouped(2)
          .map { case (nameObj @ TypeObject(StrType, _)) :: value :: Nil =>
            val key = nameObj.stringTag("value")
            key -> value.tpe
          }
          .toList
        TypeObject(StructType("", "", obj))
      }),
      "getMember" -> RuntimeValue.Func({
        case TypeObject(s: StructType, _) :: keyArg :: Nil =>
          val key = keyArg match {
            case s: String => s
            case keyObj @ TypeObject(StrType, _) =>
              keyObj.stringTag("value")
          }
          TypeObject(s.args.find(_._1 == key).getOrElse(error(s"key ${key} not found in struct $s"))._2)
        case other => error(s"Wrong type for $other")
      }),
      "Func" -> ((args: List[Any]) => {
        val tpes = args.collect { case t: TypeObject => t.tpe }
        TypeObject(FuncType(tpes.take(tpes.length - 1), tpes.last))
      })
    )

    val intrinsics = basicTypes ++ typeConstructors

    Env.make[RuntimeValue](
      intrinsics.map { case (name, v) => name -> (Strict(v): RuntimeValue) },
      None
    )
  }

  def typeStmts(stmts: List[Stmt]): TypeObject = Evaluator.runAll(stdEnv, stmts, new TyperVisitor())._2

}
