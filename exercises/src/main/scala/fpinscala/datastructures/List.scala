package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Returns x + y => 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightWithShortCircuit[A,B](as: List[A], z: B, y: A, default: B)(f: (A, B) => B): B = // Utility functions
  {
    println(as)
    as match {
      case Nil => z
      case Cons(`y`, _) => default
      case Cons(x, xs) => f(x, foldRightWithShortCircuit(xs, z, y, default)(f))
    }
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def productWithShortCircuit(ns: List[Double]) =
    foldRightWithShortCircuit(ns, 1.0, 0.0, 0.0)(_ * _)

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) {
      l
    } else {
      l match {
        case Nil => sys.error("Drop on a empty list")
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    // Or:
    // case Cons(h,t) if f(h) => dropWhile(t, f)
    // case _ => l
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def lengthWithoutFoldRight[A](l: List[A]): Int = {
    @annotation.tailrec
    def loop(as: List[A], acc: Int): Int = {
      as match {
        case Nil => acc
        case Cons(_, t) => loop(t, acc + 1)
      }
    }
    loop(l, 0)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, x: Int) => x + 1)
  }

  def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(xs: List[A], acc: B): B = {
      xs match {
        case Nil => acc
        case Cons(h, t) => loop(t, f(acc, h))
      }
    }

    loop(l, z)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumWithFoldLeft(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)


  def productWithFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthWithFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, i) => Cons(i, acc))

  def foldRightWithFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])((append(_, _)))

  def concat[A](as: List[List[A]]*): List[A] =
    if (as.isEmpty) Nil
    else append(foldRight(as.head, Nil: List[A])((append(_, _))), concat(as.tail: _*))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, b) => Cons(a.toString(), b))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def loop(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => {
        if (f(h)) buf += h
        loop(t)
      }
    }

    loop(l)

    List(buf.toList: _*)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))
    // or just `concat(map(l)(f))`

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addLists1(a: List[Int], b: List[Int]): List[Int] = {
    val buf = new collection.mutable.ListBuffer[Int]
    @annotation.tailrec
    def loop(a: List[Int], b: List[Int]): Unit = (a, b) match {
      case (Nil, _) | (_, Nil) => ()
      case (Cons(ha, ta), Cons(hb, tb)) => buf += ha + hb; loop(ta, tb)
    }
    loop(a, b)
    List(buf.toList: _*)
  }

  def addLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addLists(ta, tb))
    }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }

  def hasSubsequence1[A](sup: List[A], sub: List[A]): Boolean = {
    def loop_cmp(a: List[A], b: List[A]): Boolean = (a, b) match {
      case (_, Nil) => true
      case (Cons(a, at), Cons(b, bt)) if a == b => loop_cmp(at, bt)
      case _ => false
    }

    def loop_a(a: List[A]): Boolean =
      a match {
        case Nil => false
        case (Cons(_, t)) => if (loop_cmp(a, sub)) true else loop_a(t)
      }

    loop_a(sup)
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop_cmp(a: List[A], b: List[A]): Boolean = (a, b) match {
      case (_, Nil) => true
      case (Cons(a, at), Cons(b, bt)) if a == b => loop_cmp(at, bt)
      case _ => false
    }

    sup match {
      case Nil => false
      case (Cons(_, t)) => if (loop_cmp(sup, sub)) true else hasSubsequence(t, sub)
    }
  }

}

object ListMain {
  import scala.util.Try

  def main(args: Array[String]): Unit = {
    val l = List(1, 2, 3, 4, 5)

    println("tail l: " + List.tail(l))
    println("l: " + l)

    println("tail nil: " + List.tail(Nil))
    println("tail: " + List.tail(List(1, 2)))
    println("tail tail tail: " + List.tail(List.tail(List.tail(List(1, 2)))))

    println("setHead: " + List.setHead(l, 8))
    println("setHead: " + List.setHead(List(1), 3))
    println("setHead: " + List.setHead(Nil, 3))
    //sys.error("foobar")

    println("drop on empty: " + Try(List.drop(Nil, 8)))
    println("drop too much: " + Try(List.drop(List(1, 2), 8)))
    println("drop: " + List.drop(List(1, 2), 1))
    println("drop: " + List.drop(List(1, 2, 3), 2))

    println("dropWhile: " + List.dropWhile(List(1, 2, 3, 4, 5, 6, 7, 8), (x: Int) => x < 4))
    println("dropWhile2: " + List.dropWhile2(List(1, 2, 3, 4, 5, 6, 7, 8))(_ < 4))

    println("init: " + List.init(List('a', 'b', 'c', 'd')))
    println("init: " + List.init(Nil))

    println("productWithShortCircuit: " + List.productWithShortCircuit(List(1.0, 0.0, 5.0, 6.0)))
    println("productWithShortCircuit: " + List.productWithShortCircuit(List(1.0, 1.0, 5.0, 6.0)))

    println("foldRight: " + List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))

    println("lengthWithoutFoldRight: " + List.lengthWithoutFoldRight(List(1, 2, 3, 4, 5)))
    println("lengthWithoutFoldRight: " + List.lengthWithoutFoldRight(Nil:List[Int]))

    println("length: " + List.length(List(1, 2, 3, 4, 5)))
    println("length: " + List.length(Nil:List[Int]))

    println("length with foldLeft1: " + List.foldLeft1(List(1, 2, 3, 1), 0)((x, _) => x + 1))
    println("length with foldLeft: " + List.foldLeft(List(1, 2, 3, 1), 0)((x, _) => x + 1))
    println("lengthWithFoldLeft: " + List.lengthWithFoldLeft(List(1, 2, 3, 1)))
    println("productWithFoldLeft: " + List.productWithFoldLeft(List(1, 2, 3, 1)))
    println("sumWithFoldLeft: " + List.sumWithFoldLeft(List(1, 2, 3, 1)))

    /*
     foldLeft(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Nil)
     -> foldLeft(Cons(2, Cons(3, Cons(4, Nil))),       Cons(1, Nil))
     -> foldLeft(Cons(3, Cons(4, Nil)),                Cons(2, Cons(1, Nil)))
     -> foldLeft(Cons(4, Nil),                         Cons(3, Cons(2, Cons(1, Nil))))
     -> foldLeft(Nil,                                  Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))
     -> Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))

     foldRight(Cons(1, Cons(2, Cons(3, Cons(4, Nil)))), Nil)
     -> Cons(1,                         foldRight(Cons(2, Cons(3, Cons(4, Nil)))))
     -> Cons(1, Cons(2,                 foldRight(Cons(3, Cons(4, Nil)))))
     -> Cons(1, Cons(2, Cons(3,         foldRight(Cons(4, Nil)))))
     -> Cons(1, Cons(2, Cons(3, Cons(4, foldRight(Nil)))))
     -> Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

     */
    println("reverse: " + List.reverse(List(1, 2, 3, 4)))

    println("foldRightWithFoldLeft: " + List.foldRightWithFoldLeft(List(1, 2, 3), Nil:List[Int])((Cons(_, _))))
    println("foldRight: " + List.foldRight(List(1, 2, 3), Nil:List[Int])((Cons(_, _))))
    println("foldRightWithFoldLeft: " + List.foldRightWithFoldLeft(List(1, 2, 3), 0)(_ + _))

    println("foldLeftWithFoldRight: " + List.foldLeftWithFoldRight(List(1, 2, 3), Nil:List[Int])((b, a) => Cons(a, b)))
    println("foldLeft: " + List.foldLeft(List(1, 2, 3), Nil:List[Int])((b, a) => Cons(a, b)))
    println("foldLeftWithFoldRight: " + List.foldLeftWithFoldRight(List(1, 2, 3), 0)(_ + _))

    println("appendWithFold: " + List.appendWithFold(List(1, 2, 3), List(3, 4, 5)))

    println("concat: " + List.concat(List(List(1, 2), List(3, 4), List(5, 6)), List(List(11, 12), List(13, 14), List(15, 16))))
    println("concat: " + List.concat(List(List(1, 2), List(3, 4), List(5, 6), List(11, 12), List(13, 14), List(15, 16))))

    println("addOne: " + List.addOne(List(1, 2, 3, 4, 5)))
    println("doubleToString: " + (List.doubleToString(List(1.0, 2, 3, 4, 5)):List[String]))

    println("map: " + List.map(List(1.0, 2, 3, 4, 5))((x) => x * x))

    println("filter: " + List.filter(List(1.0, 2, 3, 4, 5))(_ % 2 == 0))

    println("flatMap: " + List.flatMap(List(1, 2, 3, 4, 5))(i => List(i, i)))

    println("filterWithFlatMap: " + List.filterWithFlatMap(List(1.0, 2, 3, 4, 5))(_ % 2 == 0))

    println("addLists: " + List.addLists(List(1, 2, 3, 4, 5), List(4, 3, 2, 1, 0)))
    println("addLists: " + List.addLists(List(1), List(4, 3, 2, 1, 0)))
    println("addLists: " + List.addLists(List(1, 2, 3, 4, 5), List(4)))

    println("zipWith: " + List.zipWith(List(1, 2, 3, 4, 5), List(4, 3, 2, 1, 0))((a, b) => a + b))

    println("zipWith: " + List.zipWith(List(1, 2, 3), List("foo", "bar", "abcd"))((a: Int, b: String) => b(a)))

    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4), List(4)))
    println("hasSubsequence: " + List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)))
  }
}
