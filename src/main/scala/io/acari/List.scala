package io.acari

/**
  * Forged in the flames of battle by alex.
  */
sealed trait List[+T]

case object Nil extends List[Nothing]

case class Cons[+T](head: T, tail: List[T]) extends List[T]


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

  def apply[T](as: T*): List[T] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def pop[T](list: List[T]): List[T] = list match {
    case Cons(x, xs) => xs
  }

  def peek[T](list: List[T]): T = list match {
    case Cons(x, xs) => x
  }

  def drop[T](list: List[T], n: Int): List[T] =
    if (n < 2) pop(list)
    else drop(pop(list), n - 1)

  def dropWhile[T](list: List[T], shouldRemove: T => Boolean): List[T] =
    if (shouldRemove(peek(list))) dropWhile(pop(list), shouldRemove)
    else list

  def init[T](list: List[T]): List[T] = list match {
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(x, Cons(y, z)) => Cons(x, init(Cons(y, z)))
  }

  //TODO: MAKE TAIL RECURSIVE
  def reduceRight[T, U](list: List[T], u: U)(f: (T, U) => U): U =
    list match {
      case Nil => u
      case Cons(x, xs) => f(x, reduceRight(xs, u)(f))
    }

  def sumBetter(numbers: List[Int]) =
    reduceRight(numbers, 0)(_ + _)

  def productBetter(numbers: List[Double]) =
    reduceRight(numbers, 1D)(_ * _)

  def length[T](list: List[T]): Int =
    reduceRight(list, 0)((t, u) => u + 1)

  def reduceLeft[T, U](list: List[T], u: U)(f: (U, T) => U): U =
    list match {
      case Nil => u
      case Cons(x, xs) => reduceLeft(xs, f(u, x))(f)
    }

  def reverseList[T](list: List[T]): List[T] =
    reduceLeft[T, List[T]](list, Nil)((u, t) => Cons(t, u))

  def sumEvenBetter(ints: List[Int]): Int =
    reduceLeft(ints, 0)(_ + _)

  def productEvenBetter(doubles: List[Double]): Double =
    reduceLeft(doubles, 1D)(_ * _)

  def lengthEvenBetter[T](list: List[T]): Int =
    reduceLeft(list, 0)((u, t) => u + 1)

  def append[T](list: List[T], t: T): List[T] =
    Cons(t, list)

  def appendList[T](list: List[T], otherList: List[T]): List[T] =
    reduceRight(list, otherList)((t, u) => Cons(t, u))

  def flatMap[T](list: List[List[T]]): List[T] =
    reduceRight[List[T], List[T]](list, Nil)(appendList)

  def map[T, U](list: List[T])(f: (T) => U): List[U] =
    reduceRight(list, Nil: List[U])((t, v) => Cons(f(t), v))

  def filter[T](list: List[T])(f: (T => Boolean)): List[T] =
    reduceRight(list, Nil: List[T])((t, u) => if (f(t)) Cons(t, u) else u)

  def flatMap2[T, U](list: List[T])(f: T => List[U]): List[U] =
    reduceRight(list, Nil: List[U])((t, u) => appendList(f(t), u))

  def zipInt(list: List[Int], list2: List[Int]): List[Int] =
    zipIntHelper(list, list2, Nil)(_ + _)

  def zipWith[T, U](list: List[T], list2: List[T])(f: (T, T) => U): List[U] =
    zipIntHelper(list, list2, Nil)(f)

  override def main(args: Array[String]): Unit = {

    val result: Int = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(result)

    val fibo = List(5, 4, 3, 2, 1)
    val removeOne: List[Int] = fibo match {
      case Cons(x, xs) => xs
    }
    println(removeOne)

    val swapOne = List("me me", "big", "boy") match {
      case Cons(x, xs) => Cons("Me Me", xs)
    }

    println(swapOne)


    val dropTheBase = drop(List("ohh", "dats", "vewy", "nice"), 2)

    println(dropTheBase)

    val vewyNice = dropWhile[String](List("ohh", "dats", "vewy", "nice"),
      !_.equals("nice"))

    println(vewyNice)

    val allButOne = init(fibo)
    println(allButOne)

    val summyBoi = sumBetter(fibo)
    println(summyBoi)

    val michealDubles = List(1d, 2d, 3d, 4d, 5d)
    val producto = productBetter(michealDubles)
    println(producto)

    val shorty = List(1, 2, 3)
    println(reduceRight(shorty, Nil: List[Int])(Cons(_, _)))

    println(length(fibo))

    println(sumEvenBetter(fibo))
    println(productEvenBetter(michealDubles))
    println(lengthEvenBetter(fibo))
    println(reverseList(fibo))
    println(append(fibo, 6))

    val listOfLists = List(List("Three", "To the One", "Two the One", "To the Three"),
      List("Here", "Come", "Dat", "Boi"),
      List("Me", "Me", "Big", "Boy"))
    println(listOfLists)
    println(flatMap(listOfLists))


    println(reduceRight[Double, List[Double]]
      (michealDubles, Nil)((t, u) => Cons(t + 1, u)))

    println(reduceRight[Double, List[String]]
      (michealDubles, Nil)((t, u) => Cons(t.toString, u)))

    println(map(fibo)(i => i + 5L))

    println(filter(fibo)((d) => d % 2 != 0))

    println(flatMap2(fibo)(d => List(d, d, d)))

    println(zipInt(fibo, shorty))
  }

  private def zipIntHelper[T, U](list: List[T], list2: List[T], returnList: List[U])(f: (T, T) => U): List[U] =
    if (list == Nil || list2 == Nil) returnList
    else zipIntHelper(pop(list), pop(list2), append(returnList, f(peek(list), peek(list2))))(f)

  private def appendOther[T](list: List[T], returnList: List[T]) = {
    if (list != Nil)
      appendList(returnList, list)
    else returnList
  }
}