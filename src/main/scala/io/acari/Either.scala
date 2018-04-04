package io.acari

/**
  * Forged in the flames of battle by alex.
  */
sealed trait Either[+E, +T] {

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+T](value: T) extends Either[Nothing, T]