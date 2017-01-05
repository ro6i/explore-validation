package validation


abstract class Result[+A](val isValid: Boolean)

case class Valid[A](value: A) extends Result[A](true)
case class JustValid[A]() extends Result[A](true)

case class NotValid(message: Message) extends Result[Nothing](false)
case class ValueNotValid[+A](message: Message, value: A) extends Result[A](false)
