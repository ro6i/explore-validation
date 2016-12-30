import org.scalatest._
import validation._
import validation.Validator._
import validation.runner._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


class ValidationTest extends FlatSpec {

  implicit val ec: ExecutionContext = ExecutionContext.global

  case class ValidationState(memberId: Option[Int], orderId: Option[Long], someId: Option[Long], otherId: Option[Long])

  "Single entry validation" should "succeed without any message" in {
    val result =
      for {
        a <- isInt("-1a23", Error("string must represent an integer."))
      } yield a

    assert(result match {
      case Invalidated(m) =>
        assert(m.nonEmpty)
        true
      case _ => false
    })
  }
  it should "fail with a message" in {
    val result =
      for {
        a <- isInt("-123", Error("string must represent an integer."))
      } yield a

    assert(result match {
      case Validated(s, _) =>
        assert(s.get == -123)
        true
      case _ => false
    })
  }
  it should "fail succeed with a warning" in {
    val result =
      for {
        a <- isInt("-1b23", Warning("string must represent an integer."))
      } yield a

    assert(result match {
      case Validated(_, m) =>
        assert(m.head == Warning("string must represent an integer."))
        true
      case _ => false
    })
  }

  "Multiple entry validation" should "fail with an error and a warning" in {
    val result =
      for {
        a <- isInt("-123", Error("string must represent an integer."))
        b <- isLong("34", Error("string must represent a long integer."))
        d <- isLong("678", Error("string must represent a long integer."))
        _ <- custom(() => d.get < 45, Warning("amount is too high."))

        c <- isPositive(a.get, Error("member id must be positive."))

    } yield ValidationState(a, b, c, d)

    assert(result match {
      case Invalidated(m) =>
        assert(m == (Error("member id must be positive.") :: Warning("amount is too high.") :: Nil))
        true
      case _ => false
    })
  }

  "Multiple entry validation" should "succeed with a warning" in {
    val result =
      for {
        a <- isInt("-123", Error("string must represent an integer."))
        b <- isLong("34", Error("string must represent a long integer."))
        _ <- custom(() => b.get < 34, Warning("id is greater than the threshold."))
        _ <- isPositive(a.get, Warning("member id must be positive."))

      } yield ValidationState(a, b, None, None)

    assert(result match {
      case Validated(s, m) =>
        assert(s == ValidationState(Some(-123), Some(34), None, None))
        assert(m == (Warning("member id must be positive.") :: Warning("id is greater than the threshold.") :: Nil))
        true
      case _ => false
    })
  }

  "DSL validation" should "succeed with a warning" in {

    val result = Check that
      isInt("-123", Error("string must represent an integer.")) andThen
      isLong("34a", Warning("string must represent an integer."))

    assert(result match {
      case Validated(_, m) =>
        assert(m == (Warning("string must represent an integer.") :: Nil))
        true
      case _ => false
    })
  }

  "Simple for comprehension with condition" should "succeed with a warning" in {
    val result =
      for (a <- isInt("-123", Error("string must represent an integer.")) if a.get > 5) yield a

    assert(result match {
      case Invalidated(m) =>
        assert(m == Nil)
        true
      case _ => false
    })
  }

  "Async validation" should "succeed with a warning" in {

    val result =
      for {
        a <- isInt("-123", Error("string must represent an integer."))
        b <- customAsync[Int]({ a.get + 124 }, 10.millis, _ == 1, Error("integer must equal to 1."))

      } yield (a, b)

    assert(result match {
      case Validated(s, m) =>
        assert(s._1.get == -123)
        assert(s._2.get == 1)
        assert(m == Nil)
        true
      case _ => false
    })
  }

  "Timed out async validation" should "fail with an error" in {

    val result =
      for {
        a <- isInt("-123", Error("string must represent an integer."))
        b <- customAsync[Int]({ Thread.sleep(1000); a.get + 124 }, 10.millis, _ == 1, Error("integer must be equal to 1."))

      } yield (a, b)

    assert(result match {
      case Invalidated(m) =>
        assert(m.size == 1)
        true
      case _ => false
    })
  }
}
