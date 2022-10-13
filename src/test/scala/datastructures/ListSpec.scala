package datastructures

import fpinscala.datastructures.List
import fpinscala.datastructures.Nil
import org.scalatest.funspec.AnyFunSpec

class ListSpec extends AnyFunSpec {

  describe("EXERCISE3.2") {
    val list = List(1, 2, 3, 4, 5)

    it("should be List(2, 3, 4, 5)") {
      val expected = List(2, 3, 4, 5)
      assert(expected == List.tail(list))
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
      assert(expected == List.setHead(list, 10))
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
      assert(expected == List.drop(input, 3))
    }

    it("should not raised error when List is empty") {
      val input    = List()
      val expected = List()
      assert(expected == List.drop(input, 3))
    }

    it("should be List() when -1 is specified for drop") {
      val input    = List()
      val expected = List()
      assert(expected == List.drop(input, -1))
    }
  }

  describe("EXERCISE3.5") {
    val f: Int => Boolean = x => x <= 3

    it("should be List()") {
      val input    = List()
      val expected = List()
      assert(expected == List.dropWhile(input, f))
    }

    it("should be List(4, 5) If you drop numbers <= 3") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = List(4, 5)
      assert(expected == List.dropWhile(input, f))
    }

    it("should be List() If you drop numbers <= 3") {
      val input    = List(1, 2, 3)
      val expected = List()
      assert(expected == List.dropWhile(input, f))
    }

    it("should be List(4, 5, 6) If you drop numbers <= 3") {
      val input    = List(3, 4, 5, 6)
      val expected = List(4, 5, 6)
      assert(expected == List.dropWhile(input, f))
    }

    it("should be List(4, 5, 2) if drop only the matched range from the top of the list") {
      val input    = List(3, 4, 5, 2)
      val expected = List(4, 5, 2)
      assert(expected == List.dropWhile(input, f))
    }
  }

  describe("EXERCISE3.6") {
    it("should be List(1, 2, 3)") {
      val input    = List(1, 2, 3, 4)
      val expected = List(1, 2, 3)
      assert(expected == List.init(input))
    }

    it("should be List()") {
      val input    = List(1)
      val expected = List()
      assert(expected == List.init(input))
    }

    it("should be List() If List is empty") {
      val input    = List()
      val expected = List()
      assert(expected == List.init(input))
    }
  }
}
