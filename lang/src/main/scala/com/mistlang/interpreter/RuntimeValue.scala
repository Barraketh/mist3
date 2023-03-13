package com.mistlang.interpreter

sealed trait RuntimeValue[+A]
object RuntimeValue {
  case class Value[A](a: A) extends RuntimeValue[A]
  case object UnitVal extends RuntimeValue[Nothing]
  case class Func[A](f: List[RuntimeValue[A]] => RuntimeValue[A]) extends RuntimeValue[A]
}
