package io.acari

sealed trait Tree[+T]

case class Leaf[T](value: T) extends Tree[T]

case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree extends App {

  def size[T](root: Tree[T]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(root: Tree[Int]): Int = root match {
    case Leaf(_) => _
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[T](root: Tree[T]): Int = root match {
    case Leaf(_)=> 1;
    case Branch(left, right) => (depth(left) max depth(right)) + 1
  }

  override def main(args: Array[String]): Unit = {

  }
}