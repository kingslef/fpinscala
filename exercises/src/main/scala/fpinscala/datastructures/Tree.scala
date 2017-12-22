package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def count[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + count(l) + count(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  // These lambdas would be a bit more readable as (_ + _ + 1)

  def countWithFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a, b) => a + b + 1)

  def maximumWithFold(t: Tree[Int]): Int =
    fold(t)(a => a)((a, b) => a max b)

  def depthWithFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a, b) => (a max b) + 1)

  def mapWithFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2): Tree[B])
}


object TreeMain {
  def main(args: Array[String]): Unit = {
    println("count: " + Tree.count(Branch(Leaf(1), Leaf(2))))
    println("count: " + Tree.count(Leaf(1)))

    println("maximum: " + Tree.maximum(Branch(
      Branch(
        Branch(
          Branch(Leaf(10), Leaf(12)),
          Branch(Leaf(1), Leaf(102))),
        Branch(Leaf(0), Leaf(100))),
      Branch(Leaf(11), Leaf(10)))))

    println("depth: " + Tree.depth(Leaf(10)))
    println("depth: " + Tree.depth(Branch(Leaf(1), Leaf(2))))
    println("depth: " + Tree.depth(
      Branch(
        Branch(
          Branch(
            Branch(
              Leaf(10), Leaf(12)),
            Branch(
              Leaf(1), Leaf(102))),
          Branch(
            Leaf(0), Leaf(100))),
        Branch(
          Leaf(11), Leaf(10)))))

    println("map: " + Tree.map(Leaf("foo"))(_.length))
    println("map: " + Tree.map(Branch(Leaf("foobar"), Leaf("baba")))(_.length))

    println("countWithFold: " + Tree.countWithFold(Branch(Leaf(1), Leaf(2))))
    println("countWithFold: " + Tree.countWithFold(Leaf(1)))

    println("maximumWithFold: " + Tree.maximumWithFold(
      Branch(
        Branch(
          Branch(
            Branch(Leaf(10), Leaf(12)),
            Branch(Leaf(1), Leaf(102))),
          Branch(Leaf(0), Leaf(100))),
        Branch(Leaf(11), Leaf(10)))))

    println("depthWithFold: " + Tree.depthWithFold(Leaf(10)))
    println("depthWithFold: " + Tree.depthWithFold(Branch(Leaf(1), Leaf(2))))
    println("depthWithFold: " + Tree.depthWithFold(
      Branch(
        Branch(
          Branch(
            Branch(
              Leaf(10), Leaf(12)),
            Branch(
              Leaf(1), Leaf(102))),
          Branch(
            Leaf(0), Leaf(100))),
        Branch(
          Leaf(11), Leaf(10)))))

    println("mapWithFold: " + Tree.mapWithFold(Leaf("foo"))(_.length))
    println("mapWithFold: " + Tree.mapWithFold(Branch(Leaf("foobar"), Leaf("baba")))(_.length))
  }
}
