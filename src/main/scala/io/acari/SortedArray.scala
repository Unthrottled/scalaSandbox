package io.acari

object SortedArray extends App {
  override def main(args: Array[String]): Unit = {
    val comparator: (String, String) => Boolean = (a: String, b: String) => a.compareTo(b) <= 0
    println(isSorted(Array("a", "b", "c"),
      comparator))
    println(isSorted(Array("a", "b", "b"),
      comparator))
    println(isSorted(Array("a", "c", "b"),
      comparator))
  }

  def isSorted[T](as: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }
}