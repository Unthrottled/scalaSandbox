object Fib extends App {

  def fibonnaci(n: Int): Int = {
    def go(n: Int): Int =
      if (n == 0) 0
      else if (n == 1) 1
      else go(n - 2) + go(n - 1)

    go(n)
  }

  override def main(args: Array[String]): Unit =
    println(fibonnaci(7))
}