package com.mistlang.lang

class Env[T](map: Map[String, ValueHolder[T]], parent: Option[Env[T]]) {
  private def put(name: String, value: ValueHolder[T]): Env[T] = {
    if (map.contains(name))
      throw new RuntimeException(s"$name already defined")
    new Env(map + (name -> value), parent)
  }
  def put(name: String, value: T): Env[T] = put(name, Strict(value))

  def putLazy(name: String, value: () => T): Env[T] = put(name, new Lazy[T](value))

  def putTopLevel(toPut: Iterable[(String, Env[T] => T)]): Env[T] = {
    var nextEnv = this
    toPut.foreach { case (name, eval) =>
      nextEnv = nextEnv.putLazy(name, () => eval(nextEnv))
    }
    nextEnv
  }

  def get(name: String): Option[T] = {
    map.get(name).map(_.value).orElse(parent.flatMap(_.get(name)))
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
      if (initializing)
        throw new RuntimeException(
          "Encountered loop during lazy initialization. If you are defining a recursive function, please set an out type"
        )
      initializing = true
      val out = expr()
      res = Some(out)
      out
    }
  }
}
