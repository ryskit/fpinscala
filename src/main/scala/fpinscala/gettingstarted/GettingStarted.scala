package fpinscala.gettingstarted

import scala.annotation.tailrec

object GettingStarted {

  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 5, factorial))
    println(formatResult("fib", 5, fib))
    val ordered = (x: Int, y: Int) => x < y
    println(isSorted(Array(1, 2, 3, 4, 5), ordered))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  // B型を受け取ってC型を返す関数、を返す関数
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a, b)

  // EXERCISE2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, current: Int): Int = {
      if (n <= 0) prev
      else
        go(n - 1, current, prev + current)
    }
    go(n, 0, 1)
  }

  // EXERCISE2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(a: Seq[A]): Boolean = {
      a match {
        case Seq(h1, h2, tails @ _*) =>
          if (ordered(h1, h2)) loop(h2 +: tails)
          else false
        case _ => true
      }
    }
    loop(as)
  }

  // EXERCISE2.3
  // 与えられたシグネチャ
  // def curry[A, B, C](f: (A, B) => C): A => (B => C) = ???

  // EXERCISE2.3 ANSWER
  // Aを受け取って、Bを受け取って、Cを返す関数を定義してあげれば良い
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)

  // DEMO:
  // val sum = (a: Int, b: Int) => a + b
  // val c = curry(sum)
  // val c1 = c(1)
  // println(c1(2))
  // 3

  // EXERCISE2.4
  // 与えられたシグネチャ
  // def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???

  // EXERCISE2.4 ANSWER
  // 型の通りに素直に定義してあげれば良い
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  // DEMO
  // val sum2 = (a: Int) => (b: Int) => a + b
  // val uc = uncurry(sum2)
  // or val uc= uncurry((a: Int) => (b: Int) => a + b)
  // println(uc(1, 2))

  // EXERCISE2.5
  // 与えられたシグネチャ
  // def compose[A, B, C](f: B => C, g: A => B): A => C = ???

  // EXERCISE2.5 ANSWER
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  // DEMO
  // val f = (x: Double) => math.Pi / 2 - x
  // val cos = f andThen math.sin
  // println(cos(2.0))
}
