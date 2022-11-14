package com.mistlang.lang

object RuntimeIntrinsics {
  private def as[T](a: Any): T = a.asInstanceOf[T]

  private def f2[A, B](f: (A, B) => Any) = (l: List[Any]) => {
    f(as[A](l.head), as[B](l(1)))
  }

  val intrinsics: Map[String, Any] = Map(
    "+" -> f2[Int, Int]((a, b) => a + b),
    "-" -> f2[Int, Int]((a, b) => a - b),
    "*" -> f2[Int, Int]((a, b) => a * b),
    "==" -> f2[Any, Any]((a, b) => a == b),
    "if" -> ((l: List[Any]) => {
      if (as[Boolean](l.head)) as[Function1[List[Any], Any]](l(1)).apply(Nil)
      else as[Function1[List[Any], Any]](l(2)).apply(Nil)
    }),
    "Unit" -> ()
  )
}
