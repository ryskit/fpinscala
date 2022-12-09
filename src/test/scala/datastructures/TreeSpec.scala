package datastructures

import fpinscala.datastructures.{Branch, Leaf, Tree}
import org.scalatest.funspec.AnyFunSpec

class TreeSpec extends AnyFunSpec {

  describe("EXERCISE3.25") {
    it("node number is 1") {
      val tree     = Leaf(1)
      val expected = 1
      assert(Tree.size(tree) == expected)
    }

    it("node number is 3") {
      val tree     = Branch(Leaf(1), Leaf(2))
      val expected = 3
      assert(Tree.size(tree) == expected)
    }

    it("node number is 4") {
      val tree     = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      val expected = 7
      assert(Tree.size(tree) == expected)
    }

    it("node number is 11") {
      val tree     = Branch(Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      val expected = 11
      assert(Tree.size(tree) == expected)
    }
  }

  describe("EXERCISE3.26") {
    it("max number is 1") {
      val tree     = Leaf(1)
      val expected = 1
      assert(Tree.maximum(tree) == expected)
    }

    it("max number is 2") {
      val tree     = Branch(Leaf(1), Leaf(2))
      val expected = 2
      assert(Tree.maximum(tree) == expected)
    }

    it("max number is 4") {
      val tree     = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      val expected = 4
      assert(Tree.maximum(tree) == expected)
    }

    it("max number is 11") {
      val tree     = Branch(Branch(Branch(Leaf(2), Leaf(3)), Leaf(11)), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
      val expected = 11
      assert(Tree.maximum(tree) == expected)
    }
  }
}