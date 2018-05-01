trait Stream[+A] {
  def uncons: scala.Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case scala.None => Nil
    case scala.Some((h, t)) => h :: t.toList
  }

  def take(n: Int): Stream[A] =
    Stream.unfold((n, this))(s =>
      if (s._1 < 1) scala.None else s._2.uncons match {
        case scala.Some((a, as)) =>
          scala.Some((a, (s._1 - 1, as)))
        case scala.None => scala.None
      })

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case scala.Some((h, t)) => f(h, t.foldRight(z)(f))
      case scala.None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s.uncons match {
      case scala.Some((h, t)) if p(h) => scala.Some((h, t))
      case _ => scala.None
    })

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.uncons match {
      case scala.Some((a, as)) => scala.Some((f(a), as))
      case scala.None => scala.None
    })

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](b: B): Stream[B] =
    foldRight(Stream(b))((a, bs) => Stream.cons(a, bs))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, bs) =>
      f(a).foldRight(bs)((b, l) => Stream.cons(b, l)))

  def zip[B](that: Stream[B]): Stream[(A, B)] =
    Stream.unfold((this, that))(s => s._1.uncons match {
      case scala.Some((a, as)) => s._2.uncons match {
        case scala.Some((b, bs)) => Some(((a, b), (as, bs)))
        case scala.None => scala.None
      }
      case scala.None => scala.None
    })

  def zipAll[A1 >: A, B](that: Stream[B],
                         a0: A1, b0: B): Stream[(A1, B)] =
    Stream.unfold((this, that))(s => s._1.uncons match {
      case scala.Some((a, as)) =>
        s._2.uncons match {
          case scala.Some((b, bs)) =>
            scala.Some(((a, b), (as, bs)))
          case scala.None =>
            scala.Some(((a, b0), (as, Stream.empty)))
        }
      case scala.None => s._2.uncons match {
        case scala.Some((b, bs)) =>
          scala.Some(((a0, b), (Stream.empty, bs)))
        case scala.None => scala.None
      }
    })

  // todo with zip and forAll
  def startsWith[A1 >: A](that: Stream[A1]): Boolean =
    Stream.unfold((this, that))(s => s._2.uncons match {
      case scala.Some((h, t)) => s._1.uncons match {
        case scala.Some((a, as)) =>
          if (h == a)
            scala.Some(false, (as, t)) else scala.None
        case _ => scala.None
      }
      case _ => scala.Some(true, (Stream.empty, Stream.empty))
    }).exists(_ == true)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(s => s.uncons match {
      case scala.Some((_, as)) => scala.Some(s, as)
      case _ => scala.None
    }).append(Stream.empty)

  def hasSubsequence[A1 >: A](s1: Stream[A1],
                              s2: Stream[A1]): Boolean =
    s1.tails exists (_.startsWith(s2))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val b1 = f(a, b._1)
      (b1, Stream.cons(b1, b._2))
    })._2

}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = scala.None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = scala.Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.unfold(a)(_ => scala.Some((a, a)))

  def from(n: Int): Stream[Int] =
    Stream.unfold(n)(a => scala.Some(a, a + 1))

  def fibs: Stream[Int] =
    Stream.unfold((0, 1))(s =>
      scala.Some((s._1, (s._2, s._1 + s._2))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(p => Stream.cons(p._1, unfold(p._2)(f)))
      .getOrElse(Stream.empty)

}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => run(s) match {
      case (a, s1) => f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])
                (f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(f(a, _)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))(_.map2(_)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[A] = State[RNG, A]

  def simple(seed: Long): RNG = new RNG {
    override def toString = "RNG:simple:" + seed

    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))

    }
  }

  val int: Rand[Int] = State(_.nextInt)

  def randomPair: Rand[(Int, Int)] = int.map2(int)((_, _))

  def positiveInt: Rand[Int] =
    int.flatMap(i => // todo return same if +
      if (Int.MinValue != 0) int.map(_.abs) else positiveInt)


  def double: Rand[Double] =
    int.map(_.abs.toDouble / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] = int.map2(double)((_, _))

  def doubleInt: Rand[(Double, Int)] =
    intDouble.map(p => (p._2, p._1))

  def double3: Rand[(Double, Double, Double)] =
    double.map2(double)((_, _))
      .map2(double)((p, d) => (p._1, p._2, d))

  def ints(count: Int): Rand[List[Int]] =
    State.sequence(List.fill(count)(int))

  def positiveMax(n: Int): Rand[Int] =
    double.map(d => (d * (n + 1).toDouble).toInt)

}


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

  def unsized: SGen[A] = Unsized(this)
}

trait SGen[+A]
case class Sized[+A](forSize: Size => Gen[A]) extends SGen[A]
case class Unsized[+A](get: Gen[A]) extends SGen[A]

object Gen {
  type Domain[+A] = Stream[Option[A]]

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a), bounded(Stream(a)))

  def randomListOf[A](a: Gen[A]): Gen[List[A]] =
    a.listOfN(choose(0, 11))

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

  val charlist = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')

  def character: Gen[Char] =
    choose(0, charlist.size).map(charlist(_))

  def string: Gen[String] =
    randomListOf(character).map(_.mkString)
}

trait Status

case object Exhausted extends Status

case object Proven extends Status

case object Unfalsified extends Status

type Size = Int
type MaxSize = Int
type TestCases = Int
type FailedCase = String
type SuccessCount = Int
type Result = Either[FailedCase, (Status, SuccessCount)]

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (m, t, rng) =>
    run(m, t, rng) match {
      case Right((s1, c1)) => p.run(m, t, rng) match {
        case Right((s2, c2)) =>
          if (s1 == s2 && s1 == Proven) Right((Proven, c1 + c2))
          else Right((Unfalsified, c1 + c2))
        case failed => failed
      }
      case failed => failed
    }
  }

  def ||(p: Prop): Prop = Prop { (m, t, rng) =>
    run(m, t, rng) match {
      case r@Right(_) => r
      case failed1 => p.run(m, t, rng) match {
        case r@Right(_) => r
        case failed2 => failed2
      }
    }
  }
}

object Prop {
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
      val result: Result = props.map(p => Prop {
        (m, _, r) => p.run(m, casesPerSize, r)
      }).reduceLeft(_ && _)(max, n, rng)
      result
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

    (m, n, rng) => {
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
    }
  }

}


def print[A](s: State[RNG, A], l: RNG) = s.run(l)._1
def print[A](g: Gen[A], l: RNG) = g.sample.run(l)._1
def print[A](g: Gen[A]) =
  g.exhaustive.filter(_.isDefined).map(_.get).toList
val rng = RNG.simple(0)
val rng1 = RNG.simple(1245341)
val rng2 = RNG.simple(12431)

val smallInt = Gen.choose(-10,10)
val maxProp = Prop.forAll(Gen.listOf(smallInt)) { l =>
  val max = l.max
  !l.exists(_ > max)
}
Prop.run(maxProp)

print(Gen.uniform)
print(Gen.uniform, rng)
print(Gen.uniform, rng1)
print(Gen.uniform, rng2)
print(Gen.choose(-12.2, 5.89))
print(Gen.choose(-12.2, 5.89), rng)
print(Gen.choose(-12.2, 5.89), rng1)
print(Gen.choose(-12.2, 5.89), rng2)
print(Gen.choose(-3, 5))
print(Gen.choose(-3, 5), rng1)
print(Gen.boolean)
print(Gen.boolean, rng)
print(Gen.boolean, rng1)
print(Gen.choose(1, 1).listOfN(2))
print(Gen.choose(1, 2).listOfN(2))
print(Gen.choose(1, 4).listOfN(1))
print(Gen.choose(1, 3).listOfN(2))
print(Gen.choose(1, 3).listOfN(3))
print(Gen.choose(1, 4).listOfN(2))
print(Gen.choose(1, 3).listOfN(0))
print(Gen.choose(1, 3).listOfN(2), rng1)
print(Gen.sameParity(1, 4))
print(Gen.randomListOf(Gen.choose(1, 6)), rng1)
print(Gen.randomListOf(Gen.character), rng1)
print(Gen.integer, rng2)
print(Gen.character, rng2)
print(Gen.short, rng2)
print(Gen.string, rng2)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 5)), rng)

print(Gen.weighted((Gen.choose(0, 5), 0.5),
  (Gen.choose(5, 10), 0.5)), rng2)
print(Gen.weighted((Gen.choose(0, 5), 0.3),
  (Gen.choose(5, 10), 0.7)), rng2)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 5)), rng1)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 6)))



