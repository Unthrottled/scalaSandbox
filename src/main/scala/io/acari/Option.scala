package io.acari

sealed trait Option[+T]
case class Some[+T](get: T) extends Option[T]
case object None extends Option[Nothing]

