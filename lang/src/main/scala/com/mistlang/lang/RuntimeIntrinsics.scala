package com.mistlang.lang

import com.mistlang.lang.RuntimeValue._

object RuntimeIntrinsics {
  private def fromEnvValue(e: RuntimeValue): Any = e match {
    case StrVal(value)  => value
    case IntVal(value)  => value
    case BoolVal(value) => value
    case TupleVal(arr)  => arr
    case UnitVal        => ()
    case _              => throw new RuntimeException(s"Invalid value $e")
  }

  private def toEnvValue(a: Any): RuntimeValue = a match {
    case s: String             => StrVal(s)
    case i: Int                => IntVal(i)
    case b: Boolean            => BoolVal(b)
    case l: List[RuntimeValue] => TupleVal(l)
    case ()                    => UnitVal
    case other                 => throw new RuntimeException(s"Can't convert $other to env value")
  }

  private def as[T](a: Any): T = a.asInstanceOf[T]

  private def f1[A](f: A => Any) = FuncVal(
    Some(1),
    (l: List[RuntimeValue]) => toEnvValue(f(as[A](fromEnvValue(l.head))))
  )

  private def f2[A, B](f: (A, B) => Any) = FuncVal(
    Some(2),
    (l: List[RuntimeValue]) => toEnvValue(f(as[A](fromEnvValue(l.head)), as[B](fromEnvValue(l(1)))))
  )

  val intrinsics: Map[String, FuncVal] = Map(
    "println" -> f1[Any](println),
    "+" -> f2[Int, Int]((a, b) => a + b),
    "-" -> f2[Int, Int]((a, b) => a - b),
    "*" -> f2[Int, Int]((a, b) => a * b),
    "==" -> f2[Any, Any]((a, b) => a == b),
    "at" -> FuncVal(
      Some(2),
      (l: List[RuntimeValue]) => {
        val t = as[TupleVal](l.head)
        val idx = as[IntVal](l(1))
        t.arr(idx.value)
      }
    ),
    "append" -> FuncVal(
      Some(2),
      (l: List[RuntimeValue]) => {
        val t = as[TupleVal](l.head)
        TupleVal(t.arr :+ l(1))
      }
    )
  )

}
