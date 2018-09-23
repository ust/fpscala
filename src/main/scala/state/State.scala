package state

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

  def sequence[S, A](fs: scala.List[State[S, A]]): State[S, scala.List[A]] =
    fs.foldRight(unit[S, scala.List[A]](scala.List()))(_.map2(_)(_ :: _))

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
    int.flatMap(i =>
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

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def simulate(inputs: scala.List[Input]): State[Machine, Int] =
    State(m => inputs.foldLeft(m)((m, i) => m match {
      case Machine(_, 0, _) => m
      case Machine(false, candies, coins) => i match {
        case Turn => Machine(true, candies - 1, coins)
        case _ => m
      }
      case Machine(true, candies, coins) => i match {
        case Coin => Machine(false, candies, coins + 1)
        case _ => m
      }
    }) match {
      case m => (m.coins, m)
    })
}
