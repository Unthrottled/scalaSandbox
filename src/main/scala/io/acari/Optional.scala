package io.acari

/**
  * Forged in the flames of battle by alex.
  *
  */
object Optional extends App {

  def variance(seq: Seq[Double]): Option[Double] =
    calucluateMean(seq)(_+_)
    .flatMap(mean =>
      calucluateMean(0 +: seq)((t, u)=> t + Math.pow(u - mean, 2)))


  private def calucluateMean[T](seq: Seq[T])(f: (T, T) => Double): Option[Double] =
    if (seq.isEmpty) None
    else
      Some(seq.reduce((a, b)=> f(a,b)))
        .map(v => v / seq.length)


  override def main(args: Array[String]): Unit = {
    println(variance(Seq(1,2,3,4,5)).getOrElse(-1D))

  }
}
