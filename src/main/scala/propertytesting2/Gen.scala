package propertytesting2

import lazyness.Stream
import state.{RNG, State}
import Gen._
import state.RNG.positiveInt

case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => g.map(f(a, _)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample), Gen.flatMap(exhaustive)(f(_).exhaustive))
}

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def flatMap[A, B](d: Domain[A])(f: A => Domain[B]): Domain[B] =
    d.flatMap(_.map(f(_)).getOrElse(Stream(None)))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = n match {
    case 0 => Gen.unit(Nil)
    case _ => g.flatMap(a => listOfN(n - 1, g).map(a :: _))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val sampler = RNG.int.map(_.abs % (stopExclusive - start) + start)
    val exhaustive = Stream.unfold(start) {
      case s if s < stopExclusive => Some((s, s + 1))
      case _ => None
    }
    Gen(sampler, bounded(exhaustive))
  }

  def int: Gen[Int] = Gen(RNG.int, bounded(Stream.unfold(Int.MinValue) {
    case s if s != Int.MaxValue => Some((s, s + 1))
    case _ => None
  }))

  lazy val boolean: Gen[Boolean] = choose(0, 2).map(_ != 0)

  lazy val double: Gen[Double] = Gen(RNG.double, unbounded)

  lazy val uniform: Gen[Double] =
    Gen(positiveInt.map(_ / (Int.MaxValue.toDouble + 1)), unbounded)

  def choose(i: Double, j: Double): Gen[Double] = uniform.map(_ * (j - i) + i)
}

import propertytesting2.Prop._

trait Prop {
  def &&(p: Prop): Prop = {
    val prop = this
    new Prop {
      override def check: Either[FailedCase, SuccessCount] = ??? //prop.check && p.check
    }
    prop
  }

  def check: Either[FailedCase, SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
