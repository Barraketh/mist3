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
