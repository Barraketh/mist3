package com.mistlang.lang

case class Env[T](map: Map[String, EnvValue[T]], parent: Option[Env[T]]) {
  def put(name: String, value: T, mutable: Boolean = false): Env[T] = {
    val toPut = if (mutable) Mutable(value) else Immutable(value)
    Env(map + (name -> toPut), parent)
  }

  def set(name: String, newValue: T): Unit = {
    getValue(name) match {
      case Some(m: Mutable[T]) => m.value = newValue
      case Some(_)             => throw new RuntimeException(s"failed to set $name - $name is immutable")
      case None                => throw new RuntimeException(s"failed to set $name - $name not found")
    }
  }

  def getValue(name: String): Option[EnvValue[T]] = map.get(name).orElse(parent.flatMap(_.getValue(name)))

  def get(name: String): Option[T] = getValue(name).map(_.value)

  def newScope: Env[T] = new Env[T](Map.empty, Some(this))
}

sealed trait EnvValue[T] {
  def value: T
}
case class Immutable[T](value: T) extends EnvValue[T]
case class Mutable[T](var value: T) extends EnvValue[T]
