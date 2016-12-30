package validation


abstract class Result[+A](val isValid: Boolean, val message: Option[Message], val value: Option[A])

case class Valid[+A](override val value: Option[A]) extends Result[A](true, None, value)

case class NotValid(message1: Message) extends Result[Nothing](false, Some(message1), None)

object Result {

  def apply[A](value: Option[A]): Result[A] = Valid(value)

  def apply[A](message: Message): Result[A] = NotValid(message)
}
