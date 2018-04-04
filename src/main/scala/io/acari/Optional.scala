package io.acari

/**
  * Forged in the flames of battle by alex.
  *
  */
object Optional extends App {

  def variance(seq: Seq[Double]): Option[Double] =
    calucluateMean(seq)(i => i)
      .flatMap(mean =>
        calucluateMean(seq)(t => Math.pow(t - mean, 2)))


  private def calucluateMean[T](seq: Seq[T])(f: T => Double): Option[Double] =
    if (seq.isEmpty) None
    else
      Some(seq.map(f).sum)
        .map(v => v / seq.length)

  def map2[T, U, R](tO: Option[T], uO: Option[U])(f: (T, U) => R): Option[R] =
    for {
      t <-tO
      u <- uO
    } yield f(t,u)

  def sequence[T](listTO: List[Option[T]]): Option[List[T]] =
    List.reduceRight(listTO, Some(Nil): Option[List[T]])((tO, uO) =>
      map2(tO, uO)((t, u) => Cons(t, u)))

  def traverse[T, U](tL: List[T])(f: T => Option[U]): Option[List[U]] = {
    List.reduceRight(tL, Some(Nil): Option[List[U]])((tO, uO)=>
    map2(f(tO), uO)((t, u)=> Cons(t, u)))
  }

  override def main(args: Array[String]): Unit = {
    val doubles = Seq(1d, 2, 3, 4, 5)
    println(variance(doubles).getOrElse(-1D))
    println(calucluateMean(doubles)(d => d).getOrElse(-1))

    val mt: Option[String] = None
    println(map2(Some("Ayy"), Some("lmao"))(_ + _).getOrElse("NO SHALL PASS!"))
    println(map2(Some("Ayy"), mt)(_ + _).getOrElse("NO SHALL PASS!"))
    println(map2(mt, Some("lmao"))(_ + _).getOrElse("NO SHALL PASS!"))
  }
}
