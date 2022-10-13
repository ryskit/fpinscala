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
    it("should be List(4, 5) if drop 3") {
      val input    = List(1, 2, 3, 4, 5)
      val expected = List(4, 5)
      assert(expected == List.drop(input, 3))
    }

    it("should be List() if drop 3") {
      val input    = List()
      val expected = List()
      assert(expected == List.drop(input, 3))
    }

    it("should be List() if drop -1") {
      val input    = List()
      val expected = List()
      assert(expected == List.drop(input, -1))
    }

  }
}
