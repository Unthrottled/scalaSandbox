object SortedArray extends App {
  def isSorted[T](as: Array[T], ordered: (T, T) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: int): Boolean {
      if (n >= as.length) true
      else if (! ordered (as (n), as (n + 1) ) ) false
      else loop (n + 1)
    }

    loop(0)
  }
}