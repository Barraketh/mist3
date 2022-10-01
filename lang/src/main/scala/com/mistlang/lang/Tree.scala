package com.mistlang.lang

sealed trait Tree[+A] {
  def data: A
  def children: List[Tree[A]]
  def map[B](f: A => B): Tree[B]
}

case class Leaf[+A](data: A) extends Tree[A] {
  def children: List[Tree[A]] = Nil
  def map[B](f: A => B): Tree[B] = Leaf(f(data))
}

case class Node[+A](data: A, children: List[Tree[A]]) extends Tree[A] {
  def map[B](f: A => B): Tree[B] = Node(f(data), children.map(_.map(f)))
}
