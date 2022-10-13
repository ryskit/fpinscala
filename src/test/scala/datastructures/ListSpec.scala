package datastructures

import fpinscala.datastructures.List
import fpinscala.datastructures.Nil
import org.scalatest.funspec.AnyFunSpec

class ListSpec extends AnyFunSpec {

  describe("EXERCISE3.2") {
    val list = List(1, 2, 3, 4, 5)

    it("should be List(2, 3, 4, 5)") {
      val expected = List(2, 3, 4, 5)
      assert(List.tail(list) == expected)
    }

    it("should be throw Exception") {
      assertThrows[RuntimeException] {
        List.tail(Nil)
      }
    }
  }

  describe("EXERCISE3.3") {
    val list = List(1, 2, 3, 4, 5)
    it("should be List(10, 2, 3, 4, 5)") {
      val expected = List(10, 2, 3, 4, 5)
      assert(List.setHead(list, 10) == expected)
    }

    it("should be throw Exception") {
      assertThrows[RuntimeException] {
        List.setHead(Nil, 10)
      }
    }
  }

  describe("EXERCISE3.4") {
    it("should be List(4, 5) if drop elements(1, 2, 3)") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = List(4, 5)
      assert(List.drop(input, 3) == expected)
    }

    it("should not raised error when List is empty") {
      val input    = List()
      val expected = List()
      assert(List.drop(input, 3) == expected)
    }

    it("should be List() when -1 is specified for drop") {
      val input    = List()
      val expected = List()
      assert(List.drop(input, -1) == expected)
    }
  }

  describe("EXERCISE3.5") {
    val f: Int => Boolean = x => x <= 3

    it("should be List()") {
      val input    = List()
      val expected = List()
      assert(List.dropWhile(input, f) == expected)
    }

    it("should be List(4, 5) If you drop numbers <= 3") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = List(4, 5)
      assert(List.dropWhile(input, f) == expected)
    }

    it("should be List() If you drop numbers <= 3") {
      val input    = List(1, 2, 3)
      val expected = List()
      assert(List.dropWhile(input, f) == expected)
    }

    it("should be List(4, 5, 6) If you drop numbers <= 3") {
      val input    = List(3, 4, 5, 6)
      val expected = List(4, 5, 6)
      assert(List.dropWhile(input, f) == expected)
    }

    it("should be List(4, 5, 2) if drop only the matched range from the top of the list") {
      val input    = List(3, 4, 5, 2)
      val expected = List(4, 5, 2)
      assert(List.dropWhile(input, f) == expected)
    }
  }

  describe("EXERCISE3.6") {
    it("should be List(1, 2, 3)") {
      val input    = List(1, 2, 3, 4)
      val expected = List(1, 2, 3)
      assert(List.init(input) == expected)
    }

    it("should be List()") {
      val input    = List(1)
      val expected = List()
      assert(List.init(input) == expected)
    }

    it("should throw UnsupportedOperationException") {
      val input = List()
      assertThrows[UnsupportedOperationException] {
        List.init(input)
      }
    }
  }

  describe("EXERCISE3.6`") {
    it("should be List(1, 2, 3)") {
      val input    = List(1, 2, 3, 4)
      val expected = List(1, 2, 3)
      assert(List.init2(input) == expected)
    }

    it("should be List()") {
      val input    = List(1)
      val expected = List()
      assert(List.init2(input) == expected)
    }

    it("should throw UnsupportedOperationException") {
      val input = List()
      assertThrows[UnsupportedOperationException] {
        List.init2(input)
      }
    }
  }
}
