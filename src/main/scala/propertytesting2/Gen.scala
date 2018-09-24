package propertytesting2

import lazyness.Stream
import state.{RNG, State}

case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(f))

}

object Gen {

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream(a))

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    def epic(streamList: List[Stream[A]]): List[Stream[A]] = ???

    a.map(List(_))
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val sampler = RNG.int.map(_.abs % (stopExclusive - start) + start)
    val exhaustive = Stream.unfold(start) {
      case s if s < stopExclusive - 1 => Some((s, s + 1))
      case _ => None
    }
    Gen(sampler, exhaustive)
  }

  def int: Gen[Int] = Gen(RNG.int, Stream.unfold(Int.MinValue) {
    case s if s != Int.MaxValue => Some((s, s + 1))
    case _ => None
  })

  lazy val boolean: Gen[Boolean] = choose(0, 2).map(_ != 0)
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
