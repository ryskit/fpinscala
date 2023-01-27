package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(v) => Right(f(v))
      case Left(e)  => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(v) => f(v)
      case Left(e)  => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(v) => Right(v)
      case Left(_)  => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}
case class Left[+E](value: E)  extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  // EXERCISE4.7
  // Eitherでsequenceとtraverseを実装せよ。
  // これらは、エラーが発生した場合に、最初に検出されたエラーを返すものとする。
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(e => e)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil)) { case (v, acc) =>
      f(v).map2(acc)(_ :: _)
    }
}

object Main extends App {
  val l = List(Right("a"), Right("a"), Right("a"), Right("a"), Right("a"), Left(new Exception), Right("x"))
  println(Either.sequence(l))
}
