package io.acari

/**
  * Forged in the flames of battle by alex.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List extends App {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def pop[A](list: List[A]): List[A] = list match {
    case Cons(x, xs) => xs
  }

  def drop[A](list: List[A], n: Int): List[A] =
    if (n < 2) pop(list)
    else drop(pop(list), n - 1)


  override def main(args: Array[String]): Unit = {

    val result: Int = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(result)

    val removeOne: List[Int] = List(5, 4, 3, 2, 1) match {
      case Cons(x, xs) => xs
    }
    println(removeOne)

    val swapOne = List("me me", "big", "boy") match {
      case Cons(x, xs) => Cons("Me Me", xs)
    }

    println(swapOne)


    val dropTheBase = drop(List("ohh", "dats", "vewy", "nice"), 2)

    println(dropTheBase)

  }
}