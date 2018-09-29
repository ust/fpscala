package propertytesting2

import lazyness.Stream
import propertytesting2.Gen._
import state.RNG.positiveInt
import state.{RNG, State}

case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => g.map(f(a, _)))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample), Gen.flatMap(exhaustive)(f(_).exhaustive))

  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(s => forSize(s).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(s => forSize(s).flatMap(f(_).forSize(s)))
}

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def flatMap[A, B](d: Domain[A])(f: A => Domain[B]): Domain[B] =
    d.flatMap(_.map(f(_)).getOrElse(Stream(None)))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(
    uniform.flatMap(p => if (p < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1).sample,
    union(g1._1, g2._1).exhaustive
  )

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))

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

trait Status {
  def &&(s: Status): Status = Status.and(this)(s)
}

case object Proven extends Status

case object Exhausted extends Status

case object Unfalsified extends Status

object Status {
  def and(a: Status)(b: Status): Status = (a, b) match {
    case (Unfalsified, _) | (_, Unfalsified) => Unfalsified
    case (Exhausted, _) | (_, Exhausted) => Exhausted
    case _ => Proven
  }
}

import propertytesting2.Prop._

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) =>
      run(m, n, rng) match {
        case Right((status, cases)) => p.run(m, n, rng)
          .map(r => (status && r._1, cases + r._2))
        case l => l
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) => run(m, n, rng).fold(_ => p.run(m, n, rng), Right(_))
  }
}

object Prop {
  type MaxSize = Int
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (ma9x, n, rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]]): Result =
        if (i == j) Right((Exhausted, i))
        else s.uncons match {
          case Some((Some(h), t)) =>
            try {
              if (f(h)) go(i + 1, j, t)
              else Left(h.toString)
            }
            catch {
              case e: Exception => Left(buildMsg(h, e))
            }
          case Some((None, _)) => Right((Unfalsified, i))
          case None => Right((Proven, i))
        }

      go(0, n / 3, a.exhaustive) match {
        case r@(Left(_) | Right((Proven, _))) => r
        case _ => go(n / 3, n, randomStream(a)(rng).map(Some(_)))
          .map(r => (Unfalsified, r._2))
      }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  private[this] def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = n / max + 1
      Stream.from(0).take(max + 1).map(i => forAll(g(i))(f))
        .toList.map(p => Prop((max, _, rng) => p.run(max, casesPerSize, rng)))
        .reduceLeft(_ && _).run(max, n, rng)
    }
  }

  private[this] def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private[this] def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")
}
