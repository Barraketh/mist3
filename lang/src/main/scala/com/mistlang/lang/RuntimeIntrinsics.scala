package com.mistlang.lang

import com.mistlang.lang.EnvValue.{FuncVal, Term, TupleVal}

object RuntimeIntrinsics {
  private def value[T](a: Any): T = a.asInstanceOf[Term[Any]].value.asInstanceOf[T]
  private def as[T](a: Any): T = a.asInstanceOf[T]

  private def f1[A](f: A => Any) = FuncVal[Any](
    1,
    (l: List[EnvValue[Any]]) => Term(f(value[A](l.head)))
  )

  private def f2[A, B](f: (A, B) => Any) = FuncVal(
    2,
    (l: List[EnvValue[Any]]) => Term(f(value[A](l.head), value[B](l(1))))
  )

  val intrinsics: Map[String, FuncVal[Any]] = Map(
    "println" -> f1[Any](println),
    "+" -> f2[Int, Int]((a, b) => a + b),
    "-" -> f2[Int, Int]((a, b) => a - b),
    "*" -> f2[Int, Int]((a, b) => a * b),
    "==" -> f2[Any, Any]((a, b) => a == b),
    "at" -> FuncVal(
      2,
      (l: List[EnvValue[Any]]) => {
        val t = as[TupleVal[Any]](l.head)
        val idx = value[Int](l(1))
        t.arr(idx)
      }
    ),
    "append" -> FuncVal(
      2,
      (l: List[EnvValue[Any]]) => {
        val t = as[TupleVal[Any]](l.head)
        TupleVal(t.arr :+ l(1))
      }
    ),
    "if" -> FuncVal(
      3,
      (l: List[EnvValue[Any]]) => {
        val pred = value[Boolean](l.head)
        val success = as[FuncVal[Any]](l(1))
        val fail = as[FuncVal[Any]](l(2))
        if (pred) success.f(Nil) else fail.f(Nil)
      }
    )
  )
}
