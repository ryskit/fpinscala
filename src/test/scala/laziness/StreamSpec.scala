package laziness

import org.scalatest.funspec.AnyFunSpec
import fpinscala.laziness.Stream


class StreamSpec extends AnyFunSpec {
  describe("EXERCISE5.3") {
    it("should get 2, 4, 6, 8") {
      val expected = Stream(2, 4, 6, 8)
      assert(expected.toList == Stream(2, 4, 6, 8, 11).takeWhile(_ % 2 == 0).toList)
    }

    it("should get 2, 4, 6, 8, 10") {
      val expected = Stream(2, 4, 6, 8, 10)
      assert(expected.toList == Stream(2, 4, 6, 8, 10).takeWhile(_ % 2 == 0).toList)
    }
  }

  describe("EXERCISE5.4") {
    it("should be true if  number is greater than 0") {
      assert(Stream(1, 2, 3, 4, 5).forAll(_ > 0))
    }

    it("should be false if number is not divided by 2") {
      assert(!Stream(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
    }
  }

  describe("EXERCISE5.5") {
    it("should get 2, 4, 6, 8") {
      val expected = Stream(2, 4, 6, 8)
      assert(expected.toList == Stream(2, 4, 6, 8, 11).takeWhileUseFoldRight(_ % 2 == 0).toList)
    }

    it("should get 2, 4, 6, 8, 10") {
      val expected = Stream(2, 4, 6, 8, 10)
      assert(expected.toList == Stream(2, 4, 6, 8, 10).takeWhileUseFoldRight(_ % 2 == 0).toList)
    }
  }

  describe("EXERCISE5.6") {
    it("should be Some(1)") {
      val expected = Some(1)
      assert(expected == Stream(1, 2, 3).headOptionUseFoldRight())
    }

    it("should be None") {
      val expected = None
      assert(expected == Stream.empty.headOptionUseFoldRight())
    }
  }

  describe("EXERCISE5.7") {
    describe("map") {
      it("should be Stream(2, 4, 6, 8, 10)") {
        val expected = Stream(2, 4, 6, 8, 10)
        assert(expected.toList == Stream(1, 2, 3, 4, 5).map(_ * 2).toList)
      }
    }

    describe("filter") {
      val expected = Stream(2, 4)
      assert(expected.toList == Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList)
    }
  }
}
