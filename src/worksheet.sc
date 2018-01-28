import State.unit

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(r0 => run(r0) match {
      case (a, r1) => g(a).run(r1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](rb: State[S, B])
                (f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(f(a, _)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))(_.map2(_)(_ :: _))
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

  def randomPair: Rand[(Int, Int)] = {
    val r: Rand[Int] = State(_.nextInt)
    r.map2(r)((_, _))
  }

  def positiveInt: Rand[Int] = {
    val nextInt: Rand[Int] = State(_.nextInt)
    nextInt.flatMap(i =>
      if (Int.MinValue != 0) nextInt.map(_.abs)
      else positiveInt)
  }

  def double: Rand[Double] = {
    val nextInt: Rand[Int] = State(_.nextInt)
    nextInt.map(_.abs.toDouble / Int.MaxValue.toDouble)
  }

  def intDouble: Rand[(Int, Double)] = {
    val r: Rand[Int] = State(_.nextInt)
    r.map2(double)((_, _))
  }

  def doubleInt: Rand[(Double, Int)] =
    intDouble.map(p => (p._2, p._1))

  def double3: Rand[(Double, Double, Double)] =
    double.map2(double)((_, _))
      .map2(double)((p, d) => (p._1, p._2, d))

  def ints(count: Int): Rand[List[Int]] =
    State.sequence(List.fill(count)(State(_.nextInt)))

  def positiveMax(n: Int): Rand[Int] =
    double.map(d => (d * (n + 1).toDouble).toInt)

}

val r = RNG.simple(-1)
val ra1 = RNG.simple(-1245)
val ra2 = RNG.simple(-24345)
val ra3 = RNG.simple(-1287845)
val ri: Rand[Int] = State(_.nextInt)

RNG.ints(3)
State.sequence(ri :: ri :: ri :: Nil).run(ra1)
RNG.double.run(ra1)
RNG.positiveMax(3).run(ra1)
RNG.positiveMax(3).run(ra2)
RNG.positiveMax(3).run(ra3)
RNG.positiveMax(3).run(RNG.simple(-44546245))
val resList = RNG.ints(3).run(RNG.simple(-10123294))
resList._2.nextInt
RNG.double3.run(RNG.simple(-10123294))
RNG.doubleInt.run(RNG.simple(-1012324294))
RNG.intDouble.run(RNG.simple(-1012324294))
RNG.double.run(RNG.simple(-1012324294))
r.nextInt
RNG.positiveInt.run(r)
