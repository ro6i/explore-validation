package validation

package object runner {

  implicit def valueValidatorResult2Stage[A](validator: Validator.ValueValidator[A]): Stage[Option[A]] = {
    val result = validator()
    result match {
      case Valid(_) => Validated(result.value, Nil)
      case NotValid(m) => m match {
        case Info(_) |
             Warning(_) => Validated(result.value, m :: Nil)
        case Error(_) => Invalidated(m :: Nil)
      }
    }
  }
}
