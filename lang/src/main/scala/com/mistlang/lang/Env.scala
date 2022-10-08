package com.mistlang.lang

class Env[T](map: Map[String, T], parent: Option[Env[T]]) {
  def put(name: String, value: T): Env[T] = {
    if (map.contains(name))
      throw new RuntimeException(s"$name already defined")

    new Env(map + (name -> value), parent)
  }

  def get(name: String): Option[T] = {
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
  def apply[Ast, T](expr: Ast, env: () => Env[ValueHolder[T]], eval: (Env[ValueHolder[T]], Ast) => T) =
    new Lazy[T](() => eval(env(), expr))
}
