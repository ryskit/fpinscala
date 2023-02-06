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
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // finalにしている理由は、サブクラスでオーバーライドすると末尾再帰ではなくなる可能性があるため、
  // finalを指定しないとコンパイルエラーが発生するため。
  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // EXERCISE5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // EXERCISE5.4
  // Streamの要素のうち、指定された術後とマッチするものをすべてチェックするforAllを実装せよ。
  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) =>
      p(a) && b
    }
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
}

object Main extends App {
  println(Stream.apply(1, 2, 3, 4, 5).toList)
}
