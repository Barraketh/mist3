package com.mistlang.interpreter

case class Env[T](map: Map[String, EnvValue[T]], parent: Option[Env[T]]) {
  def put(name: String, value: T): Env[T] = {
    if (map.contains(name)) throw new RuntimeException(s"$name already defined")

    val toPut = EnvValue(value)
    Env(map + (name -> toPut), parent)
  }

  def set(name: String, newValue: T): Unit = {
    getValue(name) match {
      case Some(m) => m.value = newValue
      case None    => throw new RuntimeException(s"failed to set $name - $name not found")
    }
  }

  private def getValue(name: String): Option[EnvValue[T]] = map.get(name).orElse(parent.flatMap(_.getValue(name)))

  def get(name: String): Option[T] = getValue(name).map(_.value)

  def newScope: Env[T] = new Env[T](Map.empty, Some(this))
}

object Env {
  def empty[T]: Env[T] = new Env(Map.empty, None)

  def make[T](values: Map[String, T], parent: Option[Env[T]]): Env[T] = {
    Env[T](values.map { case (key, v) => key -> EnvValue(v) }, parent)
  }
}

case class EnvValue[T](var value: T)
