package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this map(f) match {
      case Left(e) => Left(e)
      case Right(a) => a
    }

  def flatMap2[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap(a => b map(bb => f(a, bb)))

  def map2WithFor[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h) flatMap(hh => traverse(t)(f) map(tt => hh :: tt))
  }

  def traverseWithMap2[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    // TODO: Why this doesn't work with parenthesis?
    // case h :: t => f(h) map2 traverseWithMap2(t)(f)(_ :: _)
    case h :: t => (f(h) map2 traverseWithMap2(t)(f))(_ :: _)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(i => i)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}

object EitherMain {
  def Try[A, B](a: A)(f: A => B): Either[Exception, B] = {
      try Right(f(a)) catch {
        case e: Exception => Left(e)
      }
  }

  def tryStuff() = {
    val r: Either[String, Int ] = Right(3)
    val l: Either[String, Int] = Left("error")

    println("r: " + (r map(_ + 4)))
    println("l: " + (l map(_ + 4)))

    println("r: " + (r match {
      case Right(i) => Right(i + 3)
      case Left(e) => Left(e)
    }))

    println("l: " + (l match {
      case Right(i) => Right(i + 3)
      case Left(e) => Left(e)
    }))
  }

  def main(args: Array[String]): Unit = {
    println("map: " + (Right(3) map(_ + 3)))
    println("map: " + ((Left("fail"): Either[String, Int]) map(_ + 3)))
    println("map: " + (Right("3") map((s: String) => Try(s)(_.toInt))))
    println

    println("flatMap: " + (Right("3") flatMap((s: String) => Try(s)(_.toInt))))
    println("flatMap: " + (Right("nan") flatMap((s: String) => Try(s)(_.toInt))))
    println("flatMap: " + (Left("fail") flatMap((s: String) => Try(s)(_.toInt))))
    println

    println("orElse: " + (Right(3) orElse(Right(6))))
    println("orElse: " + ((Left("fail"): Either[String, Int]) orElse(Right(8))))
    println

    println("map2: " + (Right(3).map2(Right(6))(_ + _)))
    println("map2: " + (Right(3).map2((Left("err"): Either[String, Int]))(_ + _)))
    println("map2: " + ((Left("err"): Either[String, Int]).map2((Right(3)))(_ + _)))
    println("map2: " + ((Left("err"): Either[String, Int]).map2((Left("err"): Either[String, Int]))(_ + _)))
    println

    println("traverse: " + Either.traverse(List("1", "2", "3"))(Try(_)(_.toInt)))
    println("traverse: " + Either.traverse(List("1", "foobar", "3"))(Try(_)(_.toInt)))
    println("traverse: " + Either.traverse(List("1", "foobar", "foo"))(Try(_)(_.toInt)))
    println

    println("sequence: " + Either.sequence(List(Right("1"), Right("2"), Right("3"))))
    println("sequence: " + Either.sequence(List(Right("1"), Left("Error"), Right("3"))))
    println

    tryStuff()
  }
}
