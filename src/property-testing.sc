trait Stream[+A] {
  def uncons: scala.Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: scala.List[A] = uncons match {
    case scala.None => scala.Nil
    case scala.Some((h, t)) => h :: t.toList
  }

  def take(n: Int): Stream[A] =
    Stream.unfold((n, this))(s =>
      if (s._1 < 1) scala.None else s._2.uncons match {
        case scala.Some((a, as)) => scala.Some((a, (s._1 - 1, as)))
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
        case scala.Some((b, bs)) => scala.Some(((a, b), (as, bs)))
        case scala.None => scala.None
      }
      case scala.None => scala.None
    })

  def zipAll[A1 >: A, B](that: Stream[B],
                         a0: A1, b0: B): Stream[(A1, B)] =
    Stream.unfold((this, that))(s => s._1.uncons match {
      case scala.Some((a, as)) => s._2.uncons match {
        case scala.Some((b, bs)) => scala.Some(((a, b), (as, bs)))
        case scala.None => scala.Some(((a, b0), (as, Stream.empty)))
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
          if (h == a) scala.Some(false, (as, t)) else scala.None
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

  def unfold[A, S](z: S)(f: S => scala.Option[(A, S)]): Stream[A] =
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

  def ints(count: Int): Rand[scala.List[Int]] =
    State.sequence(scala.List.fill(count)(int))

  def positiveMax(n: Int): Rand[Int] =
    double.map(d => (d * (n + 1).toDouble).toInt)

}

//type Gen[A] = State[RNG, A]

case class Gen[+A](sample: State[RNG, A], exhaustive: Stream[A])

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream(a))

  def listOf[A](a: Gen[A]): Gen[List[A]] =
    ???

  /** Generate lists of length n, using the given generator. */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)), Stream.empty)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.double
      .map(d => d * (stopExclusive.abs - start.abs) + start)
      .map(_.intValue()), Stream.empty)

  def boolean: Gen[Boolean] =
    Gen(choose(0, 2).sample.map(_ == 1), Stream.empty)
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

}

trait Prop {
  //  def check: Either[FailedCase, SuccessCount]
  def check: Either[String, Int]

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop

  def &&(p: Prop): Prop = this.check match {
    case Left(_) => this
    case _ => p
  }
}

def print[A](s: State[RNG, A], l: RNG) = s.run(l)._1
def print[A](g: Gen[A], l: RNG) = g.sample.run(l)._1
val rng = RNG.simple(0)
val rng1 = RNG.simple(1245341)
print(RNG.double, rng1)
print(Gen.choose(0, 5), rng1)
print(Gen.boolean, rng)
print(Gen.boolean, rng1)
print(Gen.listOfN(14, Gen.choose(1,6)), rng1)



