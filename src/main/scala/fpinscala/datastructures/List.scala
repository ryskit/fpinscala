package fpinscala.datastructures

import scala.collection.mutable.ListBuffer

sealed trait List[+A]
case object Nil                             extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

//class Cons[+A](val head: A, val tail: List[A]) extends List[A]
//
//object Cons {
//  def apply[A](head: A, tail: List[A]): Cons[A] = new Cons(head, tail)
//
//  def unapply[A](c: Cons[A]): Option[(A, List[A])] = {
//    println("unapplyが呼び出されたよ!!!!!!")
//    Option(c.head, c.tail)
//  }
//}

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  // EXERCISE3.2
  // Listの最初の要素を削除する関数tailを実装せよ。この関数の実行時間が一定であることに注意。
  // ListがNilである場合、実装上の選択肢として他に何があるか。この質問については、次章で再び取り上げる。
  //
  // Nilの場合はNilを返すでも良さそう
  // ScalaのListのtailの実装では、java.lang.UnsupportedOperationException: tail of empty listがスローされる
  def tail[A](l: List[A]): List[A] = l match {
    //    case Nil => Nil
    case Nil           => throw new RuntimeException("tail of empty list")
    case Cons(_, tail) => tail
  }

  // EXERCISE3.3
  // EXERCISE3.2と同じ考え方に基づいて、Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil           => throw new RuntimeException("setHead of empty list")
    case Cons(_, tail) => Cons(x, tail)
  }

  // EXERCISE3.4
  // tailを一般化して、リストの先頭からn個の要素を削除するdropという関数に書き換えよ。
  // この関数の実行時間は削除する要素の数に飲み比例することに注意。
  // List全体のコピーを作成する必要はない。
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil            => Nil
      case list if 0 >= n => list
      case Cons(_, tail)  => drop(tail, n - 1)
    }

  // EXERCISE3.5
  // 述語とマッチする場合に限り、Listからその要素までの要素を削除するdropWhileを実装せよ。
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case list                  => list
    }

  // EXERCISE3.6
  // Listの末尾を除くすべての要素で構成されたListを返すinit関数を実装せよ。
  // List(1, 2, 3, 4)が与えられた場合、initはList(1, 2, 3)を返す。
  // この関数をtailのように一定時時間で実装できないのはなぜか？
  //
  // Ans. Listは前から後ろに向けての単一方向のリストの構造のため、init関数のような末尾を削除する操作を行いたい場合、
  // 前からListを計算していくしかないため、一定時間では実装できない。（つまり、Listの要素が多ければ多いほど処理に時間がかかる）
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil              => throw new UnsupportedOperationException("init of empty list") // Nilでも良さそう
      case Cons(_, Nil)     => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def init2[A](l: List[A]): List[A] = {
    val acc = new ListBuffer[A]()
    @scala.annotation.tailrec
    def go(list: List[A]): List[A] =
      list match {
        case Nil          => throw new UnsupportedOperationException("init of empty list") // Nilでも良さそう
        case Cons(_, Nil) => List(acc.toList: _*)
        case Cons(head, tail) =>
          acc.addOne(head)
          go(tail)
      }
    go(l)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  // List(1.0, 2.0, 3.0)
  // f(1.0, foldRight(List(2.0, 3.0), 1.0)(f))
  // f(1.0, f(2.0, foldRight(List(3.0), 1.0)(f)))
  // f(1.0, f(2.0, f(3.0, foldRight(Nil, 1.0)(f)))
  // f(1.0, f(2.0, f(3.0, 1.0)))
  // f(1.0, f(2.0, 3.0 * 1.0))
  // f(1.0, f(2.0, 3.0))
  // f(1.0, 6.0)
  // 6.0

  // EXERCISE3.7
  // 1) foldRightを使って実装されたproductは、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか。 その理由を説明せよ。
  // 2) 大きなリストでfoldRightを呼び出した場合の短絡の仕組みについて検討せよ。
  //
  // 1) ただちに再帰を中止して0.0を返せない。
  // 理由は、fが実行される前にすべての要素を辿ってしまうため。
  // 2) このfoldRightは末尾再帰で実装されていないため、コールスタックが溢れてスタックオーバーフローが発生すると思う
}

object Main extends App {
  // EXERCISE3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + List.sum(t)
    case _                                     => 101
  }
  println(x)

}
