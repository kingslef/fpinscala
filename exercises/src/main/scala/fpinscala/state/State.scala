package fpinscala.state

import scala.math

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, rng2) if i < 0 => (-(i + 1), rng2)
      case n => n
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), r2)
  }

  def double2(rng: RNG): (Double, RNG) =
    nonNegativeInt(rng) match {
      case (i, rng2) => (i.toDouble / (Int.MaxValue.toDouble + 1), rng2)
    }

  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0)
      (Nil, rng)
    else {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count - 1)(rng2)
      (i :: l, rng3)
    }
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

object StateMain {
  def main(args: Array[String]): Unit = {
    val simple = RNG.Simple(2)

    val (i, _) = RNG.nonNegativeInt(simple)
    val (i2, rng2) = RNG.nonNegativeInt(simple)
    val (i3, _) = RNG.nonNegativeInt(rng2)

    println(s"$i = $i2, $i3")

    val (ints, _) = RNG.ints(10)(simple)
    println(s"ints: ${ints}")

    val (d, r1) = RNG.double(simple)
    val (d2, r2) = RNG.double(r1)
    val (d3, r3) = RNG.double(r2)
    val (d4, r4) = RNG.double(r3)

    println(s"double: $d $d2 $d3 $d4")
  }
}
