package io.acari

/**
  * Forged in the flames of battle by alex.
  */
sealed trait Either[+E, +T] {
  def orElse[EE >: E, U >: T](u: => Either[EE, U]): Either[EE, U] = this match {
    case Left(_) => u
    case Right(_) => this
  }

  def map2[EE >: E, U, V](uE: Either[EE, U])(f: (T, U) => V): Either[EE, V] =
    flatMap(t => uE.map(u => f(t, u)))

  def sequence[E, T](es: List[Either[E, T]]): Either[E, List[T]] =
    List.reduceRight[Either[E, T], Either[E, List[T]]](es, Right(Nil))((e, lE) =>
      e.flatMap(t => lE.map(list => Cons(t, list))))

  def map[U](f: T => U): Either[E, U] = this match {
    case Left(e) => Left(e)
    case Right(t) => Right(f(t))
  }

  def flatMap[EE >: E, U](f: T => Either[EE, U]): Either[EE, U] = this match {
    case Left(e) => Left(e)
    case Right(t) => f(t)
  }

  def traverse[E, T, U](es: List[Either[E, T]])(f: T => Either[E, U]): Either[E, List[U]] =
    List.reduceRight[Either[E, T], Either[E, List[U]]](es, Right(Nil))((e, lE) =>
      e.flatMap(t => f(t))
        .flatMap(u => lE.map(list => Cons(u, list))))
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+T](value: T) extends Either[Nothing, T]