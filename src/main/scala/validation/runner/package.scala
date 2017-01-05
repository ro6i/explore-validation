package validation

import java.time.LocalDate


package object runner {

  implicit def valueValidatorResult2Stage[A](validator: Validator.ValueValidator[Option[A]]): Stage[Option[A]] = {
    val result = validator()
    result match {
      case Valid(value) => Validated(value, Nil)
      case JustValid() => Validated(None, Nil)

      case ValueNotValid(m, v) => m match {
        case Info(_) |
             Warning(_) => Validated(v, m :: Nil)
        case Error(_) => Invalidated(m :: Nil)
      }

      case NotValid(m) => m match {
        case Info(_) |
             Warning(_) => Validated(None, m :: Nil)
        case Error(_) => Invalidated(m :: Nil)
      }
    }
  }

  implicit def valueValidatorResult2Stage2[A](validator: Validator.ValueValidator[A]): Stage[A] = {
    val result = validator()
    result match {
      case Valid(value) => Validated(value, Nil)
      case JustValid() => Validated[A](null.asInstanceOf[A], Nil)

      case ValueNotValid(m, v) => m match {
        case Info(_) |
             Warning(_) => Validated(v, m :: Nil)
        case Error(_) => Invalidated(m :: Nil)
      }

      case NotValid(m) => m match {
        case Info(_) |
             Warning(_) => Validated(null.asInstanceOf[A], m :: Nil)
        case Error(_) => Invalidated(m :: Nil)
      }
    }
  }

  implicit val stringToBooleanConverter: String => Boolean = java.lang.Boolean.parseBoolean
  implicit val stringToCharConverter: String => Char = _(0)
  implicit val stringToByteConverter: String => Byte = java.lang.Byte.parseByte
  implicit val stringToShortConverter: String => Short = java.lang.Short.parseShort
  implicit val stringToIntConverter: String => Int = java.lang.Integer.parseInt
  implicit val stringToLongConverter: String => Long = java.lang.Long.parseLong
  implicit val stringToFloatConverter: String => Float = java.lang.Float.parseFloat
  implicit val stringToDoubleConverter: String => Double = java.lang.Double.parseDouble
  implicit val stringToLocalDateConverter: String => LocalDate = java.time.LocalDate.parse
  implicit val stringToLocalDateWithFormatConverter: (String, java.time.format.DateTimeFormatter) => LocalDate =
    (s: String, f: java.time.format.DateTimeFormatter) => java.time.LocalDate.parse(s, f)
}
