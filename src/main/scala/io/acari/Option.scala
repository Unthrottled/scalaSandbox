package io.acari

sealed trait Option[+T]{
  def map[U](f: T => U): Option[U]
  def flatMap[U](f: T => Option[U]): Option[U]
  def getOrElse[U >: T](default: => U): U
  def orElse[U >: T](ob: => Option[U]): Option[U]
  def filter(f: T => Boolean): Option[T]
}
case class Some[+T](get: T) extends Option[T] {
  override def map[U](f: T => U): Option[U] = ???

  override def flatMap[U](f: T => Option[U]): Option[U] = ???

  override def getOrElse[U >: T](default: => U): U = ???

  override def orElse[U >: T](ob: => Option[U]): Option[U] = ???

  override def filter(f: T => Boolean): Option[T] = ???
}
case object None extends Option[Nothing] {
  override def map[U](f: Nothing => U): Option[U] = ???

  override def flatMap[U](f: Nothing => Option[U]): Option[U] = ???

  override def getOrElse[U >: Nothing](default: => U): U = ???

  override def orElse[U >: Nothing](ob: => Option[U]): Option[U] = ???

  override def filter(f: Nothing => Boolean): Option[Nothing] = ???
}


