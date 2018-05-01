package propertytesting

import lazyness.Stream
import propertytesting.Prop._
import state._

import scala.collection.immutable

case class Gen[+A](sample: State[RNG, A],
                   exhaustive: Stream[Option[A]]) {
  def flatMap[B](a: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a(_).sample),
      exhaustive.flatMap(_.map(a(_).exhaustive)
        .getOrElse(Stream.empty)))

  def map[B](a: A => B): Gen[B] =
    Gen(sample.map(a), exhaustive.map(_.map(a)))

  def map2[B, C](b: Gen[B])
                (f: (A, B) => C): Gen[C] =
    flatMap(a0 => b.map(f(a0, _)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(listOfN)

  def listOfN(n: Int): Gen[List[A]] = n match {
    case 0 => map(_ => Nil)
    case 1 => map(_ :: Nil)
    case _ => flatMap(a => listOfN(n - 1).map(a :: _))
  }

  def listOf1: Gen[List[A]] = listOfN(1)

  def unsized: SGen[A] = Unsized(this)
}

trait SGen[+A] {
  type Size = Int
}

case class Sized[+A](forSize: Int => Gen[A]) extends SGen[A] {

}

case class Unsized[+A](get: Gen[A]) extends SGen[A]

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(Stream(a)))

  def randomListOf[A](a: Gen[A]): Gen[List[A]] =
    a.listOfN(choose(0, 11))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    Sized(_ => g.listOfN(1))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    Sized(n => g.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g2 else g1)

  def weighted[A](g1: (Gen[A], Double),
                  g2: (Gen[A], Double)): Gen[A] =
    uniform.flatMap(p =>
      if (p * (g1._2 + g2._2) < g1._2) g1._1 else g2._1)

  /** Between 0 and 1, not including 1. */
  def uniform: Gen[Double] = Gen(RNG.double, unbounded)

  /** Between `i` and `j`, not including `j`. */
  def choose(i: Double, j: Double): Gen[Double] =
    uniform.map(d => d * (j - i).abs + i)

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = choose(start.toDouble,
      stopExclusive.toDouble).sample.map(_.intValue())
    val d = bounded(Stream.unfold(start)(s =>
      if (s < stopExclusive) Some((s, s + 1)) else None))
    Gen(s, d)
  }

  def boolean: Gen[Boolean] = choose(0, 2).map(_ == 1)

  def short: Gen[Short] =
    choose(Short.MinValue.toInt, Short.MaxValue.toInt)
      .map(_.toShort)

  def integer: Gen[Int] = choose(Int.MinValue, Int.MaxValue)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = {
    val g = choose(from, to)
    g.flatMap(i => g.flatMap(j => g.map(k => {
      val ie = i % 2 == 0
      val je = j % 2 == 0
      val ke = k % 2 == 0
      if (ie == je) (i, j) else if (ie == ke) (i, k) else (j, k)
    })))
  }

  val charList: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')

  def character: Gen[Char] =
    choose(0, charList.size).map(charList(_))

  def string: Gen[String] =
    randomListOf(character).map(_.mkString)
}

trait Status {
  def min(s: Status): Status = Status.min(this, s)
}

case object Proven extends Status

case object Exhausted extends Status

case object Unfalsified extends Status

object Status {
  val order = List(Unfalsified, Exhausted, Proven)

  def min(a: Status, b: Status): Status =
    if (order.indexOf(a) <= order.indexOf(b)) a else b
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (m, t, rng) =>
    run(m, t, rng) match {
      case Right((s1, c1)) => p.run(m, t, rng) match {
        case Right((s2, c2)) =>
          Right((s1 min s2, c1 + c2))
        case failed => failed
      }
      case failed => failed
    }
  }

  def ||(p: Prop): Prop = Prop { (m, t, rng) =>
    run(m, t, rng) match {
      case r@Right(_) => r
      case _ => p.run(m, t, rng) match {
        case r@Right(_) => r
        case l => l
      }
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Sized(p) => forAll(p)(f)
    case Unsized(p) => forAll(p)(f)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = n / max + 1
      val props: List[Prop] =
        Stream.from(0).take(max + 1)
          .map(i => forAll(g(i))(f)).toList
      val mapper: Prop => Prop =
        p => Prop { (m, _, r) => p.run(m, casesPerSize, r) }

      props.map(mapper).reduceLeft(_ && _).run(max, n, rng)
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    def go(i: Int, j: Int,
           s: Stream[Option[A]],
           onEnd: Int => Result): Result =
      if (i == j) Right((Unfalsified, i))
      else s.uncons match {
        case Some((Some(h), t)) =>
          try {
            if (f(h)) go(i + 1, j, t, onEnd) // maybe s->t ?
            else Left(h.toString)
          }
          catch {
            case e: Exception => Left(buildMsg(h, e))
          }
        case Some((None, _)) => Right((Unfalsified, i))
        case None => onEnd(i)
      }

    (_, n, rng) => {
      go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  /** Produce an infinite random stream from a `Gen`
    * and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases = 100,
          rng: RNG = RNG.simple(System.currentTimeMillis)): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Left(msg) => println("! test failed:\n" + msg)
      case Right((Unfalsified, n)) =>
        println("+ property unfalsified, ran " + n + " tests")
      case Right((Proven, n)) =>
        println("+ property proven, ran " + n + " tests")
      case Right((Exhausted, n)) =>
        println("+ property unfalsified up to max size, ran " +
          n + " tests")
      case r => println("Unexpected state: " + r)
    }
  }

}
