package errorhandling

import org.scalatest.funspec.AnyFunSpec
import fpinscala.errorhandling._

class OptionSpec extends AnyFunSpec {

  describe("EXERCISE4.3") {
    it("should Some(AB)") {
      val ao: Option[String]       = Some("A")
      val bo: Option[String]       = Some("B")
      val expected: Option[String] = Some("AB")
      assert(Option.map2(ao, bo)((a, b) => a + b) === expected)
    }

    it("should None because ao is None") {
      val ao: Option[String]       = None
      val bo: Option[String]       = Some("B")
      val expected: Option[String] = None
      assert(Option.map2(bo, ao)((a, b) => a + b) === expected)
    }

    it("should None because bo is None") {
      val ao: Option[String]       = Some("A")
      val bo: Option[String]       = None
      val expected: Option[String] = None
      assert(Option.map2(bo, ao)((a, b) => a + b) === expected)
    }

    it("should None because both of None") {
      val ao: Option[String]       = None
      val bo: Option[String]       = None
      val expected: Option[String] = None
      assert(Option.map2(bo, ao)((a, b) => a + b) === expected)
    }
  }
}
