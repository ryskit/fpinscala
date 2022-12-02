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

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
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

  // foldRight(List(1.0, 2.0, 3.0), 1.0)(f)
  // f(1.0, foldRight(List(2.0, 3.0), 1.0)(f))
  // f(1.0, f(2.0, foldRight(List(3.0), 1.0)(f)))
  // f(1.0, f(2.0, f(3.0, foldRight(Nil, 1.0)(f)))
  // f(1.0, f(2.0, f(3.0, 1.0)))
  // f(1.0, f(2.0, 3.0 * 1.0))
  // f(1.0, f(2.0, 3.0))
  // f(1.0, 6.0)
  // 6.0

  // foldLeft(List(1.0, 2.0, 3.0), 1.0)(f)
  // foldLeft((2.0, 3.0), 1.0)(f)
  // foldLeft((2.0, 3.0), f(1.0, 2.0))(f)
  // foldLeft((3.0), 2.0)(f)
  // foldLeft((), f(2.0, 3.0))(f)
  // foldLeft((), 6.0)(f)
  // 6.0

  // EXERCISE3.7
  // 1) foldRightを使って実装されたproductは、0.0を検出した場合に、直ちに再帰を中止して0.0を返せるか。 その理由を説明せよ。
  // 2) 大きなリストでfoldRightを呼び出した場合の短絡の仕組みについて検討せよ。
  //
  // 1) ただちに再帰を中止して0.0を返せない。
  // 理由は、fが実行される前にすべての要素を辿ってしまうため。
  // 2) このfoldRightは末尾再帰で実装されていないため、コールスタックが溢れてスタックオーバーフローが発生すると思う

  // EXERCISE3.8
  // foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_ _))のように, NilおよびCons自体をfoldRightに渡した場合はどうなるか。
  // これがfoldRightとListのデータコンストラクタとの関係について何を表していると思うか。
  //
  // 出力結果: Cons(1,Cons(2,Cons(3,Nil)))
  // ※　Nil: List[Int]としているのは、foldRightのB型がNilだとList[Nothing]と推定して type mismatch errorが発生する
  //
  // 処理過程
  //
  // foldRight(Cons(1,Cons(2,Cons(3,Nil))), Nil: List[Int])(Cons(_ _))
  // Cons(1, foldRight(Cons(2,Cons(3, Nil)), Nil: List[Int])(Cons(_, _)))
  // Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil: List[Int])(Cons(_, _))))
  // Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_, _)))))
  // Cons(1, Cons(2, Cons(3, Nil)))
  //
  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  //    as match {
  //      case Nil         => z
  //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //    }

  // EXERCISE3.9
  // foldRightを使ってリストの長さを計算せよ。
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => b + 1)

  // EXERCISE3.10
  // このfoldRightの実装は末尾再帰ではなく、リストが大きい場合はStackOverflowErrorになってしまう。これをスタックセーフではないと言う。
  // そうした状況であると家庭し、前章で説明した手法を使って、リスト再帰の総称関すfoldLeftを記述せよ。
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // foldLeftの計算過程
  // List(1, 2, 3)を左から右に向かって走査する
  // foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
  // foldLeft(Cons(2, Cons(3, Nil)), 1)(_ + _)
  // foldLeft(Cons(3, Nil), 3)(_ + _)
  // foldLeft(Nil, 6)(_ + _)
  // 6

  // foldRightの計算過程
  // List(1, 2, 3)を右から左に向かって走査する
  // f = _ + _
  // foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
  // f(1, foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _))
  // f(1, f(2, foldRight(Cons(3, Nil), 0)(_ + _)))
  // f(1, f(2, f(3, foldRight(Nil, 0)(_ + _)))
  // f(1, f(2, f(3, 0)))
  // f(1, f(2, 3 + 0))
  // f(1, f(2, 3))
  // f(1, 5)
  // 6

  // EXERCISE3.11
  // foldLeftを使ってsum, product, およｂリストの長さを計算する関するを記述せよ。
  def sumL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productL(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  // EXERCISE3.12
  //
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // EXERCISE3.13
  // [難問]: foldRightをベースとしてfoldLeftを記述することは可能か。その逆はどうか。
  // foldLeftを使ってfoldRightを実装すると、foldRightを末尾再帰的に実装することが可能となり、
  // 大きなリストでもスタックオーバーフローが発生しなくて便利です。
  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  //    as match {
  //      case Nil         => z
  //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //    }
  // List(1, 2, 3)を右から左に向かって走査する
  // f = _ + _
  // foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
  // f(1, foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _))
  // f(1, f(2, foldRight(Cons(3, Nil), 0)(_ + _)))
  // f(1, f(2, f(3, foldRight(Nil, 0)(_ + _)))
  // f(1, f(2, f(3, 0)))
  // f(1, f(2, 3 + 0))
  // f(1, f(2, 3))
  // f(1, 5)
  // 最後にzを渡している
  def foldLeftBaseFoldRight[A, B](l: List[A], z: B)(f2: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f2(b, a)))(z)

  // g = Int => Int
  // f2 = Int + Int
  // f = (a, g) => b => g(f2(b, a))
  // foldLeftBaseFoldRight(Cons(1, Cons(2, Nil)), 0)(_ + _)
  // foldRight(Cons(1, Cons(2, Nil)), Int => Int)((a, g) => b => g(f2(b, a)))(0)
  // f(1, foldRight(Cons(2, Nil), Int => Int)((a, g) => b => g(f2(b, a)))(0)
  // f(1, f(2, foldRight(Nil, Int => Int)((a, g) => b => g(f2(b, a)))(0)
  // f(1, f(2, g: Int => Int))(0)
  // 上手く計算式を書けなかった。。。。
  // f(1, (2, g: Int => Int) => b => g(f2(b, a)))(0)
  // ((1, g: Int => Int) => b => (2, g: Int => Int) => b => g(f2(b, a))))(0)
  // ((1, b => (2, b => g(f2(b, a))))(0)

  def foldRightBaseFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(List.reverse(l), z)((b, a) => f(a, b))

  // EXERCISE14
  // foldLeftまたはfoldRightをベースとしてappendを実装せよ
  def appendBaseFoldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(List.reverse(l1), l2)((b, a) => Cons(a, b))

  def appendBaseFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((a, b) => Cons(a, b))

  // EXERCISE15
  // [難問]: 複数のリストからなるリストを一つｎリストとして連結する関数を記述せよ。
  // この関数の実行時間はすべてのリストの長さの合計に対して線形になるはずである。
  // すでに定義した関数を使ってみること。
  def concat[A](l: List[List[A]]): List[A] =
    foldRightBaseFoldLeft(l, Nil: List[A])((a, b) => append(a, b))

  // EXERCISE3.16
  // 各要素に1を足すことで整数のリストを変換する関数を記述せよ。
  // 注意: これは新しいListを返す純粋関数になるはずである
  def plusOne(l: List[Int]): List[Int] =
    foldRightBaseFoldLeft(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // EXERCISE3.17
  // List[Double]の各値をStringに変換する関数を記述せよ。
  // d.toStringという式を使ってd: DoubleをStringに変換できる
  def doubleToString(l: List[Double]): List[String] =
    foldRightBaseFoldLeft(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  // EXERCISE3.18
  // リストの各要素を変更し、かつリストの構造をそのまま保つ総称関数mapを記述せよ。
  // この関数のシグネチャは以下のとおり。
  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRightBaseFoldLeft(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // EXERCISE3.19
  // 与えられた述語条件が満たされるまでリストから要素を削除するfilter関数を記述せよ。
  // この関数を使ってList[Int]から奇数をすべて削除せよ。
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRightBaseFoldLeft(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  // EXERCISE3.20
  // mapと同じような働きをするflatMap関数を記述せよ。
  // この関数は単一の結果ではなくリストを返し、そのリストは最終的な結果のリストに挿入されなければならない。
  // この関数のシグネチャは以下のとおり。
  // flatMap = map + flatten
  // val result = Option(Option(1))
  // if (result.isExists) {
  //   if (result.isExists) {
  //      println()
  //   }
  // }
  //
  // result.flatMap(
  // f = i => List(i, i)
  // List(1, 2, 3)
  // map = List(List(1, 1), List(2, 2), List(3, 3))
  // List(1, 1, 2, 2, 3, 3)
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // EXERCISE3.21
  // flatMapを使ってfilterを実装せよ。
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  // EXERCISE3.22
  // リストを2つ受け取り、対応する要素同士を足し合わせて新しいリストを生成する関数を記述せよ。
  // たとえば、List(1, 2, 3)とList(4, 5, 6)は、List(5, 7, 9)になる。
  def sumEachElements(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumEachElements(t1, t2))
    }

  // EXERCISE3.23
  // EXERCISE3.22で作成した関数を、整数または加算に限定されないように一般化せよ。
  // 一般化された関数にはzipWithという名前をつけること
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  // EXERCISE3.24
  // 例として、Listに別のListがサブシーケンスとしてふくまれているかどうかを調べるhasSubsequenceを実装せよ。
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def go(sup: List[A], sub: List[A], previous: Boolean): Boolean = {
      (sup, sub) match {
        case (Nil, _)                                             => previous
        case (_, Nil)                                             => previous
        case (Cons(supH, supT), Cons(subH, _)) if supH != subH    => go(supT, sub, false)
        case (Cons(supH, supT), Cons(subH, subT)) if supH == subH => goSuccess(supT, subT, true)
      }
    }

    def goSuccess(sup: List[A], sub: List[A], previous: Boolean): Boolean = {
      (sup, sub) match {
        case (Nil, _)                                                         => previous
        case (_, Nil)                                                         => previous
        case (Cons(supH, supT), Cons(subH, _)) if supH != subH || !previous   => goSuccess(supT, sub, false)
        case (Cons(supH, supT), Cons(subH, subT)) if supH == subH && previous => goSuccess(supT, subT, true)
      }
    }
    go(sup, sub, false)
  }
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

  val ans = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  //    as match {
  //      case Nil         => z
  //      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  //    }
  //
  // List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
  // List(1, 2, 3) = Cons(1,Cons(2,Cons(3,Nil)))
  //
  //
  //  println(List.reverse(List(1, 2, 3)))

  println(List.foldLeftBaseFoldRight(List(1, 2), 5)(_ + _))
}
