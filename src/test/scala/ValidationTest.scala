import java.time.LocalDate

import org.scalatest._
import validation._
import validation.Validator._
import validation.runner._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._


class ValidationTest extends FlatSpec {

  implicit val ec: ExecutionContext = ExecutionContext.global

  case class ValidationState(memberId: Option[Int], orderId: Option[Long], someId: Option[Long], otherId: Option[Long])

  "Single entry validation" should "succeed without any message" in {
    val result =
      for {
        a <- is[Int]("-1a23", Error("string must represent an integer."))
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
        a <- is[Int]("-123", Error("string must represent an integer."))
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
        a <- is[Int]("-1b23", Warning("string must represent an integer."))
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
        a <- is[Int]("-123", Error("string must represent an integer."))
        b <- is[Long]("34", Error("string must represent a long integer."))
        d <- is[Long]("678", Error("string must represent a long integer."))
        _ <- isTrue(() => d.get < 45, Warning("amount is too high."))

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
        a <- is[Int]("-123", Error("string must represent an integer."))
        b <- is[Long]("34", Error("string must represent a long integer."))
        _ <- isTrue(() => b.get < 34, Warning("id is greater than the threshold."))
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
      is[Int]("-123", Error("string must represent an integer.")) andThen
      is[Long]("34a", Warning("string must represent an integer."))

    assert(result match {
      case Validated(_, m) =>
        assert(m == (Warning("string must represent an integer.") :: Nil))
        true
      case _ => false
    })
  }

  "Simple for comprehension with condition" should "succeed with a warning" in {
    val result =
      for (a <- is[Int]("-123", Error("string must represent an integer.")) if a.get > 5) yield a

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
        a <- is[Int]("1", Error("string must represent an integer."))
        b <- bypass(Future{ Thread.sleep(1); 2 })
        c <- bypass(Future{ Thread.sleep(1); 4 })
        d <- await(b, 10.millis, Error("First future failed."))
        e <- await(c, 10.millis, Error("Second future failed."))
        _ <- isTrue(() => d == 2 && e == 4, Error("'d' and 'e' must be equal to 2 and respectively."))

      } yield d - a.get

    assert(result match {
      case Validated(s, m) =>
        assert(s == 1)
        assert(m == Nil)
        true
      case _ => false
    })
  }

  "Timed out async validation" should "fail with an error" in {

    val result =
      for {
        a <- is[Int]("1", Error("string must represent an integer."))
        b <- bypass(Future{ Thread.sleep(500); 2 })
        c <- await(b, 10.millis, Error("Future failed."))

      } yield (a, c)

    assert(result match {
      case Invalidated(m) =>
        assert(m == Error("Future failed.") :: Nil)
        true
      case _ => false
    })
  }
}
