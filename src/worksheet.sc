import State.unit

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => run(s) match {
      case (a, s1) => f(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

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

type Rand[A] = State[RNG, A]

object RNG {
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

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulate(inputs: List[Input]): State[Machine, Int] =
    State(m => (0, m))
}

val r = RNG.simple(-1)
val ra1 = RNG.simple(-1245)
val ra2 = RNG.simple(-24345)
val ra3 = RNG.simple(-1287845)
val ri: Rand[Int] = RNG.int

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
