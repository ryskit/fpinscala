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

  describe("EXERCISE3.9") {
    it("should be length is 5") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = 5
      assert(List.length(input) == expected)
    }

    it("should be length is -") {
      val input    = List()
      val expected = 0
      assert(List.length(input) == expected)
    }
  }

  describe("EXERCISE3.10") {
    it("should be 15") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = 15
      assert(List.foldLeft(input, 0)(_ + _) == expected)
    }

    it("should be 0") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = 0
      assert(List.foldLeft(input, 0)(_ * _) == expected)
    }

    it("should be 5") {
      val input    = Nil: List[Int]
      val expected = 5
      assert(List.foldLeft(input, 5)(_ + _) == expected)
    }
  }
}
