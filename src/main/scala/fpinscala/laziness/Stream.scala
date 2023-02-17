package fpinscala.laziness

import fpinscala.laziness.Stream._

import scala.annotation.tailrec

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  // EXERCISE5.1
  // StreamをListに変換し、それによりストリームを強制的に評価する関数を記述せよ。
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), acc :+ h())
      case Empty      => acc
    }
    go(this, List.empty)
  }

  // EXERCISE5.2
  // Streamの先頭からn個の要素を取り出す関数take(n)と、Streamの」先頭からnこの要素を好き府するdrop(n)関数を記述せよ
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  // finalにしている理由は、サブクラスでオーバーライドすると末尾再帰ではなくなる可能性があるため、
  // finalを指定しないとコンパイルエラーが発生するため。
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  // EXERCISE5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  // EXERCISE5.4
  // Streamの要素のうち、指定された術後とマッチするものをすべてチェックするforAllを実装せよ。
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b
    }

  // EXERCISE5.5
  def takeWhileUseFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) cons(a, b)
      else empty
    }

  // EXERCISE5.6
  def headOptionUseFoldRight(): Option[A] =
    foldRight(Option.empty[A]) { (a, _) =>
      Some(a)
    }

  // EXERCISE5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      cons(f(a), b)
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (a, b) =>
      if (f(a)) cons(a, b)
      else b
    }

  def append[B >: A](other: => Stream[B]): Stream[B] =
    foldRight(other) { (a, b) =>
      cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (a, b) =>
      f(a).append(b)
    }

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

}

case object Empty                                   extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // EXERCISE5.8
  // consを少し一般化し、指定された値の無限ストリームを返すconstant関数を記述せよ。
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // EXERCISE5.9
  // nで始まってn+1, n+2と続く整数の無限ストリームを生成する関数を記述せよ
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // EXERCISE5.10
  // フィボナッチ数列(0, 1, 1, 2, 3, 5, 8)の無限ストリームを生成するfibs関数を記述せよ。
  def fibs(): Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] =
      cons(prev, go(cur, prev + cur))

    go(0, 1)
  }

  // EXERCISE5.11
  // より汎用的なストリーム生成unfoldを記述せよ。
  // この関数は、初期状態に加えて、以下の状態と、生成されるストリームの次の値を生成する関数を受け取る。
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }
}

object Main extends App {
  println(Stream.apply(1, 2, 3, 4, 5).toList)
}
