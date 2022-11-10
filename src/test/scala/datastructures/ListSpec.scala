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

    it("should be length is 0") {
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
      val input: List[Int] = Nil
      val expected         = 5
      assert(List.foldLeft(input, 5)(_ + _) == expected)
    }
  }

  describe("EXERCISE3.10") {
    describe("sumL") {
      it("should be 15") {
        val input    = List(1, 2, 3, 4, 5)
        val expected = 15
        assert(List.sum(input) == expected)
      }

      it("should be 0") {
        val input: List[Int] = Nil
        val expected         = 0
        assert(List.sum(input) == expected)
      }
    }

    describe("productL") {
      it("should be 6.0") {
        val input    = List(1.0, 2.0, 3.0)
        val expected = 6.0
        assert(List.productL(input) == expected)
      }

      it("should be 1.0") {
        val input: List[Double] = Nil
        val expected            = 1.0
        assert(List.productL(input) == expected)
      }

      it("should be 0.0") {
        val input    = List(0.0, 2.0, 3.0)
        val expected = 0.0
        assert(List.productL(input) == expected)
      }
    }

    describe("lengthL") {
      it("should be length is 5") {
        val input    = List(1, 2, 3, 4, 5)
        val expected = 5
        assert(List.lengthL(input) == expected)
      }

      it("should be length is 0") {
        val input    = List()
        val expected = 0
        assert(List.lengthL(input) == expected)
      }
    }
  }

  describe("EXERCISE3.12") {
    it("should be List(3, 2, 1)") {
      val input    = List(1, 2, 3)
      val expected = List(3, 2, 1)
      assert(List.reverse(input) == expected)
    }

    it("should be List()") {
      val input    = List()
      val expected = List()
      assert(List.reverse(input) == expected)
    }
  }

  describe("EXERCISE3.14") {
    it("should be List(1, 2, 3, 4, 5, 6)") {
      val input1   = List(1, 2, 3)
      val input2   = List(4, 5, 6)
      val expected = List(1, 2, 3, 4, 5, 6)
      assert(List.appendBaseFoldLeft(input1, input2) == expected)
    }

    it("should be List(1, 2, 3)") {
      val input1   = List(1, 2, 3)
      val input2   = List()
      val expected = List(1, 2, 3)
      assert(List.appendBaseFoldLeft(input1, input2) == expected)
    }

    it("should be List(1, 2, 3)`") {
      val input1   = List()
      val input2   = List(1, 2, 3)
      val expected = List(1, 2, 3)
      assert(List.appendBaseFoldLeft(input1, input2) == expected)
    }
  }

  describe("EXERCISE3.15") {
    it("should be List(1, 2, 3, 4, 5, 6)") {
      val input    = List(List(1, 2), List(3, 4), List(5, 6))
      val expected = List(1, 2, 3, 4, 5, 6)
      assert(List.concat(input) == expected)
    }

    it("should be List(1, 2, 3, 4, 5, 6)`") {
      val input    = List(List(1, 2), List(3, 4), List(5, 6), Nil)
      val expected = List(1, 2, 3, 4, 5, 6)
      assert(List.concat(input) == expected)
    }
  }

  describe("EXERCISE3.16") {
    it("should be List(2, 3, 4)") {
      val input = List(1, 2, 3)
      val expected = List(2, 3, 4)
      assert(List.plusOne(input) === expected)
    }
  }

  describe("EXERCISE3.17") {
    it("should be List('1.0', '2.0', '3.0')") {
      val input = List(1.0, 2.0, 3.0)
      val expected = List("1.0", "2.0", "3.0")
      assert(List.doubleToString(input) === expected)
    }
  }

  describe("EXERCISE3.18") {
    it("should be List(2, 3, 4)") {
      val input = List(1, 2, 3)
      val expected = List(2, 3, 4)
      assert(List.map(input)(_ + 1) === expected)
    }

    it("should be List('1.0', '2.0', '3.0')") {
      val input = List(1.0, 2.0, 3.0)
      val expected = List("1.0", "2.0", "3.0")
      assert(List.map(input)(_.toString) === expected)
    }
  }
}
