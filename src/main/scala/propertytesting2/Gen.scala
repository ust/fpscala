package propertytesting2

class Gen[A] {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
}

import state.{RNG, State}

object Gen {
  type Gen[A] = State[RNG, A]

  def choose(start: Int, stopExclusive: Int): Gen[Int] = RNG.int.map(_.abs / (stopExclusive - start) + start)
}

import Prop._

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
