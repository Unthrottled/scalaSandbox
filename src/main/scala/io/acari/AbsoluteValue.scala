package io.acari

object AbsoluteValue extends App {

  override def main(args: Array[String]): Unit =
    println(formatAbsoluteValue(-42))

  private def formatAbsoluteValue(x: Int): String = {
    val message = "The absolute value of %d is %d"
    message.format(x, absoluteValue(x))
  }

  def absoluteValue(n: Int): Int =
    if (n < 0)
      -n
    else
      n

}