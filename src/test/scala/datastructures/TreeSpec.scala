package datastructures

import fpinscala.datastructures.{Branch, Leaf, Tree}
import org.scalatest.funspec.AnyFunSpec

class TreeSpec extends AnyFunSpec {

  describe("EXERCISE3.25") {
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
}
