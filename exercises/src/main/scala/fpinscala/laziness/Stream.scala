package fpinscala.laziness

// import scala.language.implicitConversions

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] =
    foldRight(Nil: List[A])(_ :: _)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take2(n: Int): Stream[A] =
    if (n <= 0)
      Empty
    else
      this match {
        case Cons(h, t) => Cons(h, () => t().take2(n - 1))
        case Empty => Empty
      }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case _ => None
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if (n > 0) => t().drop(n - 1)
      case _ => empty
    }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }


  def forAll2(p: A => Boolean): Boolean =
    !exists(!p(_))

  def forAll3(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption2: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }


  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def addOne[B >: A](b: => B): Stream[B] =
    foldRight(cons(b, empty))(cons(_, _))

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _ => None
    }

  def zipAll2[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, b)) {
      case (Cons(h, t), Cons(hh, tt)) => Some(((Some(h()), Some(hh())), (t(), tt())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), empty)))
      case (Empty, Cons(hh, tt)) => Some(((None, Some(hh())), (empty, tt())))
      case _ => None
    }

  def zipAll[B](b: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, b)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())) -> (t(), tt()))
      case (Cons(h, t), Empty) => Some((Some(h()), None) -> (t(), empty))
      case (Empty, Cons(hh, tt)) => Some((None, Some(hh())) -> (empty, tt()))
      case _ => None
    }

  // TODO: doesn't quite work
  def startsWith[B](s: Stream[B]): Boolean =
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) if h() == hh() => Some((true, (t(), tt())))
      case (Cons(h, t), Cons(hh, tt)) => Some(((t() startsWith s), (Empty, Empty)))
      case (Cons(h, t), Empty) => Some((true, (Empty, Empty)))
      case (Empty, Empty) => None
      case (_, _) => None
      }.forAll { a => a }
}
case object Empty extends Stream[Nothing]
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

  val onesWithUnfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val fibs: Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] =
      Stream.cons(prev, go(cur, cur + prev))
    go(0, 1)
  }

  val fibsWithUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def unfoldWithoutMatch[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse empty[A]

}

object StreamMain {
  def main(args: Array[String]): Unit = {
    val stream = () => Stream.cons({println(1); 1},
      Stream.cons({println(2); 2},
        Stream.cons({println(3); 3},
          Stream.cons({println(4); 4}, empty
          ))))
    // or
    // ({println(1); 1} #:: {println(2); 2} #:: {println(3); 3} #:: {println(4); 4} #:: Stream.empty)
    val exceptionStream = () => Stream.cons(throw new Exception("foo"), empty[Int])

    println("toList: " + Stream(1, 2, 3).toList)
    println

    println("take: " + stream().take(1).toList)
    println("take: " + Stream(1, 2, 3).take(1))
    println("take: " + Stream(1, 2, 3).take(1).toList)
    println("take: " + Stream(1, 2, 3).take(8).toList)
    println

    println("drop: " + Stream(1, 2, 3).drop(1))
    println("drop: " + Stream(1, 2, 3).drop(1).toList)
    println("drop: " + Stream(1, 2, 3).drop(8).toList)
    println

    println("takeWhile: " + Stream(1, 2, 3).takeWhile(_ < 3))
    println("takeWhile: " + Stream(1, 2, 3).takeWhile(_ < 3).toList)
    println("takeWhile: " + Stream(1, 2, 3).takeWhile(_ > 9).toList)
    println("takeWhile: " + stream().takeWhile(_ < 3).toList)
    println

    println("foldRight: " + stream().foldRight(false)((a, b) => a == 2 || b))
    println

    println("forAll: " + stream().forAll(_ < 5))
    println("forAll: " + stream().forAll(_ < 3))
    println

    println("takeWhileWithFoldRight: " + stream().takeWhileWithFoldRight(_ < 3).toList)
    println

    println("headOption: " + Stream.cons(2, exceptionStream()).headOption)
    println("headOption: " + stream().headOption)
    println("headOption: " + empty[Int].headOption)
    println

    println("map: " + stream().map("a" * _).toList)
    println("map: " + empty[Int].map("a" * _))
    println

    println("filter: " + stream().filter(_ < 3).toList)
    println("filter: " + empty[Int].filter(_ < 3).toList)
    println

    println("append: " + stream().append(exceptionStream()))
    println("append: " + stream().append(Stream.cons({println(5); 5}, empty[Int])).toList)
    println("append: " + stream().append(Stream(5, 6, 7, 8, 9)).toList)
    println("append: " + empty[Int].append(exceptionStream()))
    println("append: " + empty[Int].append(Stream.cons({println(5); 5}, empty[Int])).toList)
    println

    println("flatMap: " + stream().flatMap(Stream.ones.take(_)).toList)
    println

    println("ones: " + Stream.ones.map(_ + 9).take(20).toList)
    println

    println("ones for all: " + Stream.ones.forAll(_ != 1))
    // Doesn't terminate
    // println("ones for all: " + Stream.ones.forAll(_ == 1))
    println

    println("constant: " + Stream.constant("a").take(5).toList)
    println

    println("from: " + Stream.from(2).take(5).toList)
    println

    println("fibs: " + Stream.fibs.take(20).toList)
    println

    println("unfold: " + Stream.unfold(1)(i => Some((i, i + 1))).take(20).toList)
    println("unfold: " + Stream.unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }.take(20).toList)
    println("unfold: " + Stream.unfold("a")(a => Some((a, a))).take(20).toList)
    println

    println("onesWithUnfold: " + Stream.onesWithUnfold.map(_ + 9).take(20).toList)
    println

    println("constantWithUnfold: " + Stream.constantWithUnfold("a").take(5).toList)
    println

    println("fromWithUnfold: " + Stream.fromWithUnfold(2).take(5).toList)
    println

    println("fibsWithUnfold: " + Stream.fibsWithUnfold.take(20).toList)
    println

    println("unfoldWithoutMatch: " + Stream.unfoldWithoutMatch(1)(i => Some((i, i + 1))).take(20).toList)
    println("unfoldWithoutMatch: " + Stream.unfoldWithoutMatch((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }.take(20).toList)
    println("unfoldWithoutMatch: " + Stream.unfoldWithoutMatch("a")(a => Some((a, a))).take(20).toList)
    println

    println("mapWithUnfold: " + stream().mapWithUnfold("a" * _).toList)
    println("mapWithUnfold: " + empty[Int].mapWithUnfold("a" * _))
    println

    println("takeWithUnfold: " + stream().takeWithUnfold(1).toList)
    println("takeWithUnfold: " + Stream(1, 2, 3).takeWithUnfold(1))
    println("takeWithUnfold: " + Stream(1, 2, 3).takeWithUnfold(1).toList)
    println("takeWithUnfold: " + Stream(1, 2, 3).takeWithUnfold(8).toList)
    println

    println("takeWhileWithUnfold: " + stream().takeWhileWithUnfold(_ < 3).toList)
    println

    println("zipWith: " + stream().zipWith(ones)(_ + _).toList)
    println("zipWith: " + fibs.zipWith(stream())(_ + _).toList)
    println

    println("zipAll: " + Stream(1, 2, 3, 4).zipAll(Stream("a", "b")).toList)
    println

    println("startsWith: " + Stream(1, 2, 3, 4).startsWith(Stream(1, 2)))
    println("startsWith: " + Stream(1, 3, 4).startsWith(Stream(1, 2)))
  }
}
