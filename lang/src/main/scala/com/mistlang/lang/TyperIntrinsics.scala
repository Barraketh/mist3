package com.mistlang.lang

import com.mistlang.lang.RuntimeValue.IntVal
import com.mistlang.lang.Type.{IntType, TupleType}

object TyperIntrinsics {
  import TaggedType._

  private def getIntValues(a: TaggedType, b: TaggedType): Option[(Int, Int)] = {
    for {
      aValue <- a.tags.get("value")
      bValue <- b.tags.get("value")
      res <- (aValue, bValue) match {
        case (IntVal(av), IntVal(bv)) => Some(av -> bv)
        case _                        => None
      }
    } yield res
  }

  private def reduceIntValues(a: TaggedType, b: TaggedType, f: (Int, Int) => Int): TaggedType = {
    val newTags = getIntValues(a, b).map(a => "value" -> IntVal(f(a._1, a._2))).toMap
    TaggedType(IntType, newTags)
  }

  val intrinsics: Map[String, TaggedType] = Map(
    "==" -> basicFuncType(List(Type.Arg("a", anyType), Type.Arg("b", anyType)), boolType, false),
    "-" -> typeLevelFunc(
      List(Type.Arg("a", intType), Type.Arg("b", intType)),
      args => reduceIntValues(args(0), args(1), (a, b) => a - b)
    ),
    "*" -> typeLevelFunc(
      List(Type.Arg("a", intType), Type.Arg("b", intType)),
      args => reduceIntValues(args(0), args(1), (a, b) => a * b)
    ),
    "+" -> typeLevelFunc(
      List(Type.Arg("a", intType), Type.Arg("b", intType)),
      args => reduceIntValues(args(0), args(1), (a, b) => a + b)
    ),
    "at" -> typeLevelFunc(
      List(Type.Arg("tuple", anyType), Type.Arg("idx", intType)),
      (args: List[TaggedType]) => {
        val tt = args.head.t match {
          case t: TupleType => t
          case o            => Typer.error(s"Wrong argument type for parameter tuple: expected Tuple, got $o")
        }
        val i = args(1).tags.get("value") match {
          case Some(IntVal(value)) => value
          case Some(other)         => Typer.error(s"Unexpected value for idx - expected int, got $other")
          case None                => Typer.error(s"Value for idx not statically known")
        }
        if (i < 0 || i >= tt.arr.length)
          Typer.error(s"Invalid length $i for idx - must be between 0 and ${tt.arr.length}")

        tt.arr(i)
      }
    )
  )

  val runtimeIntrinsics: Map[String, TaggedType] = Map(
    "Int" -> intType,
    "String" -> strType,
    "Boolean" -> boolType,
    "Any" -> anyType
  )
}
