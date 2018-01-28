import RNG.flatMap

"hello world"

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S)) {
 // todo move to here
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def flatMap[S, A, B](f: State[S, A])
                      (g: A => State[S, B]): State[S, B] =
    State(r0 => f.run(r0) match {
      case (a, r1) => g(a).run(r1)
    })

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] =
    flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])
                      (f: (A, B) => C): State[S, C] =
    flatMap(ra)(a => map(rb)(f(a, _)))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))(map2(_, _)(_ :: _))


}

type Rand[A] = State[RNG, A]

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def positiveInt: Rand[Int] = {
    val nextInt: Rand[Int] = _.nextInt
    flatMap(nextInt)(i =>
      if (Int.MinValue != 0) map(nextInt)(_.abs) else positiveInt)
  }

  def double: Rand[Double] =
    map(_.nextInt)(_.abs.toDouble / Int.MaxValue.toDouble)

  def intDouble: Rand[(Int, Double)] =
    map2(_.nextInt, double)((_, _))

  def doubleInt: Rand[(Double, Int)] =
    map(intDouble)(p => (p._2, p._1))

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    double(rng) match {
      case (i1, r1) => double(r1) match {
        case (i2, r2) => double(r2) match {
          case (i3, r3) => ((i1, i2, i3), r3)
        }
      }
    }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))


  def positiveMax(n: Int): Rand[Int] =
    map(double)(d => (d * (n + 1).toDouble).toInt)


}

val r = RNG.simple(-1)
val ra1 = RNG.simple(-1245)
val ra2 = RNG.simple(-24345)
val ra3 = RNG.simple(-1287845)
val ri: Rand[Int] = _.nextInt

RNG.ints(3)
State.sequence(ri :: ri :: ri :: Nil)(ra1)
RNG.double(ra1)
RNG.positiveMax(3)(ra1)
RNG.positiveMax(3)(ra2)
RNG.positiveMax(3)(ra3)
RNG.positiveMax(3)(RNG.simple(-44546245))
val resList = RNG.ints(3)(RNG.simple(-10123294))
resList._2.nextInt
RNG.double3(RNG.simple(-10123294))
RNG.doubleInt(RNG.simple(-1012324294))
RNG.intDouble(RNG.simple(-1012324294))
RNG.double(RNG.simple(-1012324294))
r.nextInt
RNG.positiveInt(r)
