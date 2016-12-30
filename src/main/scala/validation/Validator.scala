package validation

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try


object Validator {

  type ValueValidator[+A] = () => Result[A]

  def customAsync[A](body: => A, timeout: FiniteDuration, predicate: A => Boolean, message: Message)
                    (implicit executionContext: ExecutionContext): ValueValidator[A] =
    Try(Await.result(Future(body), timeout)) match {
      case scala.util.Success(result) => custom[A](predicate, result, message)
      case scala.util.Failure(exception) => () => NotValid(Error(exception.getMessage))
  }

  def isSome[_](value: Option[_], message: Message): ValueValidator[Nothing] = () =>
    value match {
      case Some (_) => Valid (None)
      case None => NotValid(message)
    }

  def isTo[A](convert: String => A, value: String, message: Message): ValueValidator[A] = () =>
    Try(convert(value)).toOption match { case a @ Some(_) => Valid(a) case None => NotValid(message) }

  def custom[A](predicate: A => Boolean, value: A, message: Message): ValueValidator[A] =
    () => if (predicate(value)) Valid(Some(value)) else NotValid(message)

  def custom(predicate: () => Boolean, message: Message): ValueValidator[Nothing] =
    () => if (predicate()) Valid(None) else NotValid(message)

  def isPositive[A](value: A, message: Message)(implicit numeric: Numeric[A]): () => Result[Nothing] =
    () => if (numeric.gt(value, numeric.zero)) Valid(None) else NotValid(message)

  def isNotNull[A](value: A, message: Message): ValueValidator[Nothing] =
    custom(() => value != null, message)

  def isNotEmpty(value: String, message: Message): ValueValidator[Nothing] =
    () => if (value != "") Valid(None) else NotValid(message)

  def isNotEmpty[A](value: A, message: Message): ValueValidator[Nothing] =
    () =>
      if (value match {
        case null => false
        case "" => false
        case None => false
        case _ => true
      }
    ) Valid(None) else NotValid(message)

  def isBoolean(value: String, message: Message): ValueValidator[Boolean] =
    isTo(java.lang.Boolean.parseBoolean, value, message)

  def isChar(value: String, message: Message): ValueValidator[Char] =
    isTo(_ => value(0), value, message)

  def isByte(value: String, message: Message): ValueValidator[Byte] =
    isTo(java.lang.Byte.parseByte, value, message)

  def isShort(value: String, message: Message): ValueValidator[Short] =
    isTo(java.lang.Short.parseShort, value, message)

  def isInt(value: String, message: Message): ValueValidator[Int] =
    isTo(java.lang.Integer.parseInt, value, message)

  def isLong(value: String, message: Message): ValueValidator[Long] =
    isTo(java.lang.Long.parseLong, value, message)

  def isFloat(value: String, message: Message): ValueValidator[Float] =
    isTo(java.lang.Float.parseFloat, value, message)

  def isDouble(value: String, message: Message): ValueValidator[Double] =
    isTo(java.lang.Double.parseDouble, value, message)

  def isDate(value: String, message: Message): ValueValidator[java.time.LocalDate] =
    isTo(java.time.LocalDate.parse, value, message)

  def isDate(value: String, formatter: java.time.format.DateTimeFormatter, message: Message): ValueValidator[java.time.LocalDate] =
    isTo((v: String) => java.time.LocalDate.parse(v, formatter), value, message)
}
