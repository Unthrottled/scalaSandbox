object Currying extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  override def main(args: Array[String]): Unit = {
    def longToFunction = curry((a: Long, b: Int)=>a<b)
    def intToBoolean = longToFunction(69L)
    def bool = intToBoolean(420)
    println(bool)
  }
}