package io.acari.stream

/**
  * Forged in the flames of battle by alex.
  */

case object Empty extends Stream[Nothing]

case class Cons[+T](h: () => T, t: () => Stream[T]) extends Stream[T]


trait Stream[+T] {

  def apply[T](as: T*): Stream[T] =
    if (as.isEmpty) Empty
    else Cons(() => as.head, () => apply(as.tail: _*))


  def foldRight[U](z: => U)(f: (T, => U) => U): U = // The arrow `=>` in front of the argument type `U` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: T => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: T => Boolean): Option[T] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: io.acari.List[T] =
    foldRight[io.acari.List[T]](io.acari.Nil)((t, u) => io.acari.Cons(t, u))

  def toReversedList: List[T] = {
    @annotation.tailrec
    def go(stream: Stream[T], acc: List[T]): List[T] = stream match {
      case Cons(head, tail) => go(tail(), head()::acc)
      case _ => acc
    }
    go(this, List())
  }

  def take(n: Int): Stream[T] = {
    @annotation.tailrec
    def go(stream: Stream[T], acc: () => Stream[T], m: Int): Stream[T] = {
      if (m < 1) acc()
      else stream match {
        case Cons(h, t) => go(t(), () => Cons(h, acc), m - 1)
        case Empty => acc()
      }
    }

    go(this, () => Empty, n)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[T] = this match {
    case Cons(_, tail)=> if (n > 1 ) tail() drop(n -1)  else this
    case _ => this
  }

  def takeWhile(p: T => Boolean): Stream[T] = ???

  def forAll(p: T => Boolean): Boolean = ???

  def headOption: Option[T] = this match {
    case Empty => None
    case Cons(t, ts) => Some(t())
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[U](s: Stream[U]): Boolean = ???
}

object Stream extends App {
  val ones: Stream[Int] = Stream.cons(1, ones)

  /**
    * Smart Constructor, cache's head option.
    *
    * @param hd
    * @param tl
    * @tparam T
    * @return
    */
  def cons[T](hd: => T, tl: => Stream[T]): Stream[T] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[T]: Stream[T] = Empty

  def apply[T](as: T*): Stream[T] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def from(n: Int): Stream[Int] = ???

  def unfold[T, S](z: S)(f: S => Option[(T, S)]): Stream[T] = ???

  override def main(args: Array[String]): Unit = {
    val stremo = Stream(1, 2, 3, 4, 5)
    val listo = stremo.toList
    println(listo)
    val takeList = stremo.take(3).toReversedList
    println(takeList)
  }
}