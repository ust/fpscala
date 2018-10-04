package propertytesting2

import lazyness.Stream
import propertytesting2.Gen._
import state.RNG.positiveInt
import state.{RNG, State}

case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = flatMap(a => g.map(f(a, _)))

  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g) ((_, _))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample), Gen.flatDomain(exhaustive)(f(_).exhaustive))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def unsized: SGen[A] = Gen.unsized(this)
}

trait SGen[+A] {
  def map[B](f: A => B): SGen[B] = Gen.map(this)(f)

  def flatMap[B](f: A => SGen[B]): SGen[B] = Gen.flatMap(this)(f)

  def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = flatMap(a => g.map(f(a, _)))

  def **[B](s2: SGen[B]): SGen[(A, B)] = (this, s2) match {
    case (Sized(g), Sized(g2)) => Sized(n => g(n) ** g2(n))
    case (Unsized(g), Unsized(g2)) => Unsized(g ** g2)
    case (Sized(g), Unsized(g2)) => Sized(n => g(n) ** g2)
    case (Unsized(g), Sized(g2)) => Sized(n => g ** g2(n))
  }
}

case class Unsized[+A](get: Gen[A]) extends SGen[A]

case class Sized[+A](forSize: Size => Gen[A]) extends SGen[A]

object Gen {
  type Size = Int
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def map[A, B](g: SGen[A])(f: A => B): SGen[B] = g match {
    case Sized(forSize) => Sized(s => forSize(s).map(f))
    case Unsized(get) => Unsized(get.map(f))
  }

  def flatMap[A, B](g: SGen[A])(f: A => SGen[B]): SGen[B] =
  // SGen(s => forSize(s).flatMap(f(_).forSize(s)))
    ???

  def flatDomain[A, B](d: Domain[A])(f: A => Domain[B]): Domain[B] =
    d.flatMap(_.map(f(_)).getOrElse(Stream(None)))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = Gen(
    uniform.flatMap(p => if (p < g1._2 / (g1._2 + g2._2)) g1._1 else g2._1).sample,
    union(g1._1, g2._1).exhaustive
  )

  def unsized[A](g: Gen[A]): SGen[A] = Unsized(g)

  def listOf[A](g: Gen[A]): SGen[List[A]] = Sized(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] = Sized(s => listOfN(s max 1, g))

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

  private[this] val charList: Seq[Char] = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++
    List(' ', '!', '?', '.', ',')

  val character: Gen[Char] = choose(0, charList.size).map(charList(_))

  val string: SGen[String] = character.listOf.map(_.toString)

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] = g.map(i => s => s.hashCode * i)
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
  //  def &&(p: Prop): Prop = Prop {
  //    (m, n, rng) =>
  //      run(m, n, rng) match {
  //        case Right((status, cases)) => p.run(m, n, rng)
  //          .map(r => (status && r._1, cases + r._2))
  //        case l => l
  //      }
  //  }

  def &&(p: Prop) = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Right((a, n)) => p.run(max, n, rng).right.map { case (s, m) => (s, n + m) }
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

  def check(p: => Boolean): Prop = forAll(unit(()))(_ => p)

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => {
      def go(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i))
        else s.uncons match {
          case Some((Some(h), t)) =>
            try {
              if (f(h)) go(i + 1, j, t, onEnd)
              else Left(h.toString)
            }
            catch {
              case e: Exception => Left(buildMsg(h, e))
            }
          case Some((None, _)) => Right((Unfalsified, i))
          case None => onEnd(i)
        }

      go(0, n / 3, a.exhaustive, i => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          val rands = randomStream(a)(rng).map(Some(_))
          go(n / 3, n, rands, i => Right((Unfalsified, i)))
        case s => s
      }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Sized(forSize) => forAll(forSize)(f)
    case Unsized(get) => forAll(get)(f)
  }

  private[this] def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = n / max + 1
      Stream.from(0).take(max + 1).map(i => forAll(g(i))(f))
        .toList.map(p => Prop((_, _, _) => p.run(max, casesPerSize, rng)))
        .reduceLeft(_ && _).run(max, n, rng).map {
        case (Proven, c) => (Exhausted, c)
        case x => x
      }
    }
  }

  private[this] def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private[this] def buildMsg[A](s: A, e: Exception): String =
    "test case: " + s + "\n" +
      "generated an exception: " + e.getMessage + "\n" +
      "stack trace:\n" + e.getStackTrace.mkString("\n")
}
