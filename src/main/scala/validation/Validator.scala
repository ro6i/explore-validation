package validation

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


object Validator {

  type ValueValidator[A] = () => Result[A]

  def bypass[A](value: => A): ValueValidator[A] = () => Valid(value)

  def await[A](f: Future[A], timeout: FiniteDuration, message: Message)(implicit executionContext: ExecutionContext): ValueValidator[A] =
    () => Try(Await.result(f, timeout)) match {
      case Success(a) => Valid(a)
      case Failure(_) => NotValid(message)
    }

  def isTrue[A](predicate: A => Boolean, value: A, message: Message): ValueValidator[A] =
    () => if (predicate(value)) Valid(value) else NotValid(message)

  def isTrue(predicate: () => Boolean, message: Message): ValueValidator[Nothing] =
    () => if (predicate()) JustValid() else NotValid(message)

  def is[A <: AnyVal](value: String, message: Message)(implicit convert: String => A): ValueValidator[Option[A]] =
    () => Try(convert(value)).toOption match { case some @ Some(_) => Valid(some) case None => NotValid(message) }

  def is[A, F](value: String, formatter: F, message: Message)(implicit convert: (String, F) => A): ValueValidator[Option[A]] =
    () => Try(convert(value, formatter)).toOption match { case some @ Some(_) => Valid(some) case None => NotValid(message) }

  def isSome[_](value: Option[_], message: Message): ValueValidator[_] = () =>
    value match {
      case some @ Some (_) => Valid(some)
      case None => NotValid(message)
    }

  def isPositive[A](value: A, message: Message)(implicit numeric: Numeric[A]): () => Result[Nothing] =
    isTrue(() => numeric.gt(value, numeric.zero), message)

  def isNegative[A](value: A, message: Message)(implicit numeric: Numeric[A]): () => Result[Nothing] =
    isTrue(() => numeric.lt(value, numeric.zero), message)

  def isNotNull[A](value: A, message: Message): ValueValidator[Nothing] =
    isTrue(() => value != null, message)

  def isNotEmpty(value: String, message: Message): ValueValidator[Nothing] =
    isTrue(() => value != "", message)

  def isNotEmpty[A](value: A, message: Message): ValueValidator[Nothing] =
    () =>
      if (value match {
        case null => false
        case "" => false
        case None => false
        case Nil => false
        case _ => true
      })
        JustValid()
      else
        NotValid(message)
}
