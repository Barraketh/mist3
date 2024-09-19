package com.mistlang.lang

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, t) = run(s)
    (f(a), t)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, t) = run(s)
    f(a).run(t)
  }

  def runA(s: S): A = run(s)._1
}

object State {
  def traverse[S, A, B](args: List[A], f: A => State[S, B]): State[S, List[B]] = {
    args match {
      case Nil => State(s => (Nil, s))
      case head :: tail =>
        for {
          headRes <- f(head)
          tail <- traverse(tail, f)
        } yield headRes :: tail
    }
  }
}
