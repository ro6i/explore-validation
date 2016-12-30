package validation
package runner

import scala.language.experimental.macros


sealed abstract class Stage[+A](val messages: List[Message]) {

  def map[B](f: A => B): Stage[B]

  def flatMap[B](f: A => Stage[B]): Stage[B]

  final def foreach[B](f: A => B): Unit = map(f)

  def withFilter(cond: A => Boolean): Stage[A]

  def that[B](next: Stage[B]): Stage[B] = andThen(next)

  def andThen[B](next: Stage[B]): Stage[B] = flatMap(_ => next)
}

sealed case class Validated[+A](value: A, override val messages: List[Message]) extends Stage[A](messages) { self =>
  val head: A = value

  def map[B](f: A => B): Stage[B] = Validated(f(head), messages)

  def flatMap[B](f: A => Stage[B]): Stage[B] =
    f(head) match {
      case Validated(s, m) => Validated(s, m ::: messages)
      case Invalidated(m) => Invalidated(m ::: messages)
    }

  override def withFilter(cond: A => Boolean): Stage[A] = new WithFilter(cond)

  private class WithFilter(cond: A => Boolean) extends Validated[A](value, self.messages) {

    override def map[B](f: A => B): Stage[B] = if (cond(head)) self.map(f) else Invalidated(self.messages)

    override def flatMap[B](f: A => Stage[B]): Stage[B] = if (cond(head)) self.flatMap(f) else Invalidated(self.messages)
  }
}

object Check extends Validated(None, Nil)

case class Invalidated(override val messages: List[Message]) extends Stage[Nothing](messages) {

  def map[B](f: Nothing => B): Stage[B] = Invalidated(messages)

  def flatMap[B](f: Nothing => Stage[B]): Stage[B] = Invalidated(messages)

  override def withFilter(cond: Nothing => Boolean): Stage[Nothing] = this
}
