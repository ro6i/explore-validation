package validation

abstract class Message(val text: String)

final case class Info(override val text: String) extends Message(text) {
  override def toString = s"Info: $text"
}

final case class Warning(override val text: String) extends Message(text) {
  override def toString = s"Warning: $text"
}

final case class Error(override val text: String) extends Message(text) {
  override def toString = s"Error: $text"
}
