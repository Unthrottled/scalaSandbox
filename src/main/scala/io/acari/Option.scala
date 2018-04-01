package io.acari

sealed trait Option[+T]{
  def map[U](f: T => U): Option[U] = ???
  def flatMap[U](f: T => Option[U]): Option[U] = ???
  def getOrElse[U >: T](default: => U): U = ???
  def orElse[U >: T](ob: => Option[U]): Option[U] = ???
  def filter(f: T => Boolean): Option[T] = ???
}

case class Some[+T](get: T) extends Option[T]
case object None extends Option[Nothing]


