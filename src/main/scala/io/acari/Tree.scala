package io.acari

sealed trait Tree[+T]
case class Leaf[T](value: T) extends Tree[T]
case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

object Tree extends App {

  def size[T](root: Tree[T]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  override def main(args: Array[String]): Unit = {

  }
}