package propertytesting

import java.util.concurrent.{ExecutorService, Executors}

import lazyness.Stream
import parallelism.Par
import parallelism.Par.Par
import propertytesting.Gen._
import propertytesting.Prop._
import state._

case class Gen[+A](sample: State[RNG, A],
                   exhaustive: Stream[Option[A]]) {
  def flatMap[B](a: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a(_).sample),
      exhaustive.flatMap(_.map(a(_).exhaustive)
        .getOrElse(unbounded)))

  def map[B](a: A => B): Gen[B] =
    Gen(sample.map(a), exhaustive.map(_.map(a)))

  def map2[B, C](b: Gen[B])
                (f: (A, B) => C): Gen[C] =
    flatMap(a0 => b.map(f(a0, _)))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    map2(g)((_, _))

  def listOfN(n: Int): Gen[List[A]] = n match {
    case 0 => map(_ => Nil)
    case 1 => map(_ :: Nil)
    case _ => flatMap(a => listOfN(n - 1).map(a :: _))
  }

  def listOf1: Gen[List[A]] = listOfN(1)

  def listOf: SGen[A] = Gen.sized(this)

  def unsized: SGen[A] = Gen.unsized(this)
}

trait SGen[+A] {
  def map[B](f: A => B): SGen[B] = Gen.map(this)(f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = Gen.flatMap(this)(f)
}

case class Sized[+A](forSize: Size => Gen[A]) extends SGen[A]

case class Unsized[+A](get: Gen[A]) extends SGen[A]

object Gen {
  type Size = Int
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  val unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(Stream(a)))

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def sized[A](g: Gen[A]): SGen[A] = Sized(_ => g)

  def unsized[A](g: Gen[A]): SGen[A] = Unsized(g)

  def flatMap[A, B](g: SGen[A])(f: A => SGen[B]): SGen[B] =
    g match {
      case Unsized(u) => ???
      case _ => ???
    }

  def map[A, B](g: SGen[A])(f: A => B): SGen[B] = g match {
    case Unsized(u) => Unsized(u.map(f))
    case Sized(s) => Sized(s(_).map(f))
  }

  def randListOf[A](a: Gen[A]): Gen[List[A]] =
    choose(0, 11).flatMap(a.listOfN)

  def listOf1[A](g: Gen[A]): SGen[List[A]] = Sized(_ => g.listOfN(1))

  def listOf[A](g: Gen[A]): SGen[List[A]] = Sized(n => g.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g2 else g1)

  def weighted[A](g1: (Gen[A], Double),
                  g2: (Gen[A], Double)): Gen[A] =
    uniform.flatMap(p =>
      if (p * (g1._2 + g2._2) < g1._2) g1._1 else g2._1)

  /** Between 0 and 1, not including 1. */
  val uniform: Gen[Double] = Gen(RNG.double, unbounded)

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

  val boolean: Gen[Boolean] = choose(0, 2).map(_ == 1)

  val short: Gen[Short] =
    choose(Short.MinValue.toInt, Short.MaxValue.toInt)
      .map(_.toShort)

  val int: Gen[Int] = choose(Int.MinValue, Int.MaxValue)

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

  val char: Gen[Char] =
    choose(0, charList.size).map(charList(_))

  val string: Gen[String] =
    randListOf(char).map(_.mkString)

  def genIntFn[A](g: Gen[Any]): Gen[A => Int] =
    g.map(g => a => (g.hashCode + a.hashCode).hashCode)

  def genFn[A, B](g: Gen[Any])(f: Int => B): Gen[A => B] =
    genIntFn[A](g).map(i => a => f(i(a)))
}

trait Status {
  def <(s: Status): Status = Status.min(this, s)

  def >(s: Status): Status = Status.min(s, this)
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
          Right((s1 < s2, c1 + c2))
        case failed => failed
      }
      case failed => failed
    }
  }

  // fixme exhausted
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

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    def go(i: Int,
           j: Int,
           max: MaxSize,
           s: Stream[Option[A]],
           onEnd: SuccessCount => Result): Result =
      if (i == j) Right((Unfalsified, i))
      else s.uncons match {
        case Some((Some(h), t)) =>
          try {
            if (f(h)) go(i + 1, j, max, t, onEnd) // maybe s->t ?
            else Left(h.toString)
          }
          catch {
            case e: Exception => Left(buildMsg(h, e))
          }
        case Some((None, _)) => Right((Unfalsified, i))
        case None => onEnd(i)
      }

    (m, n, rng) => {
      go(0, n / 3, m, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n / 3, n, m, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
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

  def forAll[A](g: SGen[A])(p: A => Boolean): Prop = g match {
    case Sized(u) => forAll(u)(p)
    case Unsized(s) => forAll(s)(p)
  }

  import Gen._

  def check(p: => Boolean): Prop =
    forAll(unit(()))(_ => p)

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
    unit(Executors.newCachedThreadPool) -> 0.25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    def go(n: Int, p: Par[Boolean]): Par[Boolean] = {
      //println(s"go:$n") // FIXME close threads
      if (n < 1) p else Par.fork(p)
    }

    forAll(S ** g ** choose(1, 4)) {
      case s ** a ** n => go(n, f(a))(s).get
    }
  }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  /** Produce an infinite random stream from a `Gen`
    * and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")

  def run(p: Prop): Unit = run()(p)

  def run(description: String = "")(
    p: Prop,
    maxSize: MaxSize = 100,
    testCases: TestCases = 100,
    rng: RNG = RNG.simple(System.currentTimeMillis),
  ): Unit = {
    val desc = s""""$description""""
    p.run(maxSize, testCases, rng) match {
      case Left(msg) =>
        println(s"$desc test failed:$msg")
      case Right((Unfalsified, n)) =>
        println(s"+ property $desc unfalsified, ran $n tests")
      case Right((Proven, n)) =>
        println(s"+ property $desc proven, ran $n tests")
      case Right((Exhausted, n)) =>
        println(s"+ property $desc exhausted up to max size, ran $n tests")
      case r =>
        println(s" $desc Unexpected state: $r")
    }
  }

}
