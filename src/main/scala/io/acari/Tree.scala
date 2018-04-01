package io.acari

sealed trait Tree[+T]

case class Leaf[T](value: T) extends Tree[T]

case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Trees extends App {

  def size[T](root: Tree[T]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(i) => i
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[T](root: Tree[T]): Int = root match {
    case Leaf(_) => 1;
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  def map[T, U](root: Tree[T])(f: T => U): Tree[U] = root match {
    case Leaf(t) => Leaf(f(t))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def map2[T, U](root: Tree[T])(f: T => U): Tree[U] =
    fold[T, Tree[U]](root)(t => Leaf(f(t)))((left, right) => Branch(left, right))

  def fold[T, U](root: Tree[T])
                (leaf: T => U)
                (branch: (U, U) => U): U = root match {
    case Leaf(t) => leaf(t)
    case Branch(left, right) => branch(fold(left)(leaf)(branch), fold(right)(leaf)(branch))
  }

  override def main(args: Array[String]): Unit = {
    println("ayy lmao")
  }
}