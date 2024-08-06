package com.mistlang.interpreter

sealed trait RuntimeValue[T] {
  def value: T
}
object RuntimeValue {
  case class Strict[T](value: T) extends RuntimeValue[T]
  case class Lazy[T](get: () => T) extends RuntimeValue[T] {
    var computing = false
    var computed = false
    var cached: T = _

    override def value: T = {
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
}
