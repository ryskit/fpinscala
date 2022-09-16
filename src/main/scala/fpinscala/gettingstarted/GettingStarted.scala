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
          if (ordered(h1, h2)) loop(tails)
          else false
        case _ => true
      }
    }
    loop(as)
  }
}
