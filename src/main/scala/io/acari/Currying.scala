package io.acari

object Currying extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  override def main(args: Array[String]): Unit = {
    val toCurry = (a: Long, b: Int) => a < b
    val longToFunction = curry(toCurry)
    val intToBoolean = longToFunction(69L)
    val bool = intToBoolean(420)

    println(bool)

    val toUnCurry = (a: Long) => (b: Double) => b.compareTo(a.toDouble) > 0
    val lessCurry = uncurry(toUnCurry)
    println(lessCurry(420L, 69.2))
  }
}