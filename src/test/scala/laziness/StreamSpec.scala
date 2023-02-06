package laziness

import org.scalatest.funspec.AnyFunSpec
import fpinscala.laziness.{Empty, Stream}

import scala.runtime.Nothing$

class StreamSpec extends AnyFunSpec {

  describe("EXERCISE5.7") {
    it("should be true if  number is greater than 0") {
      assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    }

    it("should be false if number is not divided by 2") {
      assert(!Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
    }
  }
}
