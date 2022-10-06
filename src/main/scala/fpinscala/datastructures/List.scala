package fpinscala.datastructures

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
  def tail[A](l: List[A]): List[A] = l match {
//    case Nil => Nil
    case Nil => throw new RuntimeException("tail of empty list")
    case Cons(_, tail) => tail
  }

  // EXERCISE3.3
  // EXERCISE3.2と同じ考え方に基づいて、Listの最初の要素を別の値と置き換えるsetHead関数を実装せよ。
  def setHead[A](l: List[A], x: A): List[A] = l match {
    case Nil => throw new RuntimeException("setHead of empty list")
    case Cons(_, tail) => Cons(x, tail)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
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
