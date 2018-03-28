package io.acari

object AbsoluteValue extends App {

  def absoluteValue(n: Int): Int =
    if (n < 0)
      -n
    else
      n

  private def formatAbsoluteValue(x: Int): String ={
    val message = "The absolute value of %d is %d"
    message.format(x, absoluteValue(x))
  }


  override def main(args: Array[String]): Unit =
    println(formatAbsoluteValue(-42))

}