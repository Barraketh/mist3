package com.mistlang.lang

class Env[T](map: Map[String, ValueHolder[T]], parent: Option[Env[T]]) {
  def put(name: String, value: ValueHolder[T]): Env[T] = {
    if (map.contains(name))
      throw new RuntimeException(s"$name already defined")

    new Env(map + (name -> value), parent)
  }

  def get(name: String): Option[ValueHolder[T]] = {
    map.get(name).orElse(parent.flatMap(_.get(name)))
  }

  def newScope: Env[T] = new Env(Map.empty, Some(this))
}

object Env {
  def empty[T] = new Env[T](Map.empty, None)
}

sealed trait ValueHolder[T] {
  def value: T
}
case class Strict[T](value: T) extends ValueHolder[T]
class Lazy[T](expr: () => T) extends ValueHolder[T] {
  private var res: Option[T] = None
  private var initializing = false

  override def value: T = {
    res.getOrElse {
      if (initializing) throw new RuntimeException("Encountered loop during lazy initializataion")
      initializing = true
      val out = expr()
      res = Some(out)
      out
    }
  }
}

object Lazy {
  def apply[Ast, T](expr: Ast, env: () => Env[T], eval: (Env[T], Ast) => T) =
    new Lazy[T](() => eval(env(), expr))
}

sealed trait EnvValue[+T]

object EnvValue {
  case class Term[T](value: T) extends EnvValue[T]
  case class TupleVal[T](arr: List[EnvValue[T]]) extends EnvValue[T]
  case class FuncVal[T](numArgs: Int, f: List[EnvValue[T]] => EnvValue[T]) extends EnvValue[T]
  case object UnitVal extends EnvValue[Nothing]

}
