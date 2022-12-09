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

  // EXERCISE 3.26
  // Tree[Int]の最大の要素を返すmaximum関数を記述せよ。
  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(value)         => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  // EXERCISE 3.27
  // 2分木のルートから任意のLeafまでの最長パスを返すdepth関数を記述せよ。
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }
  }

  // EXERCISE 3.28
  // 2分木の各要素を特定の関数を使って変更するmap関数を記述せよ。
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  // EXERCISE 3.29
  // size, maximum, depth, mapを一般化し、それらの類似点を抽象化するあたら良いfold関数を記述せよ。
  // そして、このより汎用的なfold関数うを使ってそれらを再実装せよ。
  // このfold関数とListの左畳み込みおよび右畳み込みの間にある類似性を抽出することは可能か？
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def sizeByFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l, r) => 1 + l + r)
  }

  def maximumByFold(tree: Tree[Int]): Int = {
    fold(tree)(v => v)(_ max _)
  }

  def depthByFold[A](tree: Tree[A]): Int = {
    fold(tree)(_ => 1)((l, r) => 1 + (l max r))
  }

  def mapByFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
  }
}
