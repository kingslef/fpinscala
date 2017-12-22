package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap1[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def orElse1[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def filter1(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap(m => mean(xs.map(i => math.pow(i - m, 2))))

  def map21[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case (h :: t) => h flatMap(hh => sequence(t) map(tt => hh :: tt))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h, t)(_ :: _))

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case (h :: t) => f(h) flatMap(hh => traverse(t)(f) map (tt => hh :: tt))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(o => o)

}

object OptionMain {
  import scala.util.Try

  def failFn(i: Int): Int = {
    val y: Int = throw new Exception("fail")
    try {
      val x = 32
      x + y
    } catch { case e: Exception => 43 }
  }

  def main(args: Array[String]): Unit = {
    println("map: " + Some(3).map(_ + 3))
    println("map: " + None.map((x: Int) => x + 3))
    println

    println("getOrElse: " + Some(3).getOrElse(8))
    println("getOrElse: " + None.getOrElse(8))
    println

    println("orElse: " + Some(3).orElse(Some(8)))
    println("orElse: " + Some(3).orElse(None))
    println("orElse: " + None.orElse(Some(8)))
    println

    println("filter: " + Some(3).filter(_ == 3))
    println("filter: " + Some(3).filter(_ > 5))
    println("filter: " + None.filter(_ => true))
    println

    println("variance: " + Option.variance(Seq(1, 2, 3)))
    println("variance: " + Option.variance(Seq(1)))
    println("variance: " + Option.variance(Seq()))
    println

    println("map2: " + Option.map2(Some(1), Some(2))(_ + _))
    println("map2: " + Option.map2(None: Option[Int], Some(2))(_ + _))
    println

    println("sequence: " + Option.sequence(List(Some(1), Some(2), Some(3))))
    println("sequence: " + Option.sequence(List(Some(1), None, Some(3))))
    println("sequence: " + Option.sequence(List()))
    println

    println("sequence2: " + Option.sequence2(List(Some(1), Some(2), Some(3))))
    println("sequence2: " + Option.sequence2(List(Some(1), None, Some(3))))
    println("sequence2: " + Option.sequence2(List()))
    println

    println("traverse: " + Option.traverse(List(1, 2, 3))(i => if (i < 3) Some(i) else None))
    println("traverse: " + Option.traverse(List(1, 2, 3))(i => Some(i)))
    println

    println("sequenceWithTraverse: " + Option.sequenceWithTraverse(List(Some(1), Some(2), Some(3))))
    println("sequenceWithTraverse: " + Option.sequenceWithTraverse(List(Some(1), None, Some(3))))
    println("sequenceWithTraverse: " + Option.sequenceWithTraverse(List()))
    println
  }
}
