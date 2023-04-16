package com.mistlang.interpreter

sealed trait RuntimeValue {
  def value: Any
}
object RuntimeValue {
  case class Strict(value: Any) extends RuntimeValue
  case class Lazy(get: () => Any) extends RuntimeValue {
    var computing = false
    var computed = false
    var cached: Any = _

    override def value: Any = {
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

  object UnitVal

  def Func(f: PartialFunction[List[Any], Any]): Function[List[Any], Any] = l => f(l)
}
