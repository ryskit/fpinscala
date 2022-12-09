package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A)                        extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // EXERCISE 3.25
  // 2分木のノード(Leaf, Branch)の数を数えるsize関数を記述せよ
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }
}
