package com.mistlang.interpreter

sealed trait RuntimeValue[+A]
object RuntimeValue {
  sealed trait Value[+A] extends RuntimeValue[A] {
    def value: A
  }
  case class Strict[A](value: A) extends Value[A]
  case class Lazy[A](get: () => A) extends Value[A] {
    var computing = false
    var computed = false
    var cached: A = _

    override def value: A = {
      if (computed) cached
      else if (computing) throw new RuntimeException("Recursive lazy evaluation")
      else {
        computing = true
        cached = get()
        computed = true
        cached
      }

    }
  }
  case object UnitVal extends RuntimeValue[Nothing]
  case class Func[A](f: List[RuntimeValue[A]] => RuntimeValue[A]) extends RuntimeValue[A]
}
