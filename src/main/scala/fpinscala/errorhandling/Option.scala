package fpinscala.errorhandling

// B >: A
// これは、BはAの継承元であるという制約をかけているよ

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    // Option(Option[B])をgetOrElseで外側のOptionを引っ剥がす
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None      => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    // Some: Some(Some[A])をgetOrElseで引っ剥がす
    // None: そのままobが返る
    this.map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case _                   => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // EXERCISE 4.2
  // flatMapをベースとしてvariance関数を実装せよ。
  // シーケンスの平均をm, シーケンスの各要素をxとすれば分散はmath.pow(x -m , 2)の平均となる
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // EXERCISE 4.3
  // 2項関数を使ってOption型の2つの値を結合する総称関数map2を記述せよ。
  // どちらかのOption値がNoneの場合は、戻り地もNoneになる
  // シグネチャ
  // def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???
  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- ao
      b <- bo
    } yield f(a, b)

  def map2V2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    ao.flatMap(a => bo.map(b => f(a, b)))

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int]     = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

}
