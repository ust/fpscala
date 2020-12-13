package state

import monads.Monad

case class State[S, +A](run: S => (A, S)) {
  // to do a cache in companion
  private val monad = State.monad[S]

  def flatMap[B](f: A => State[S, B]): State[S, B] = monad.flatMap(this)(f)

  //flatMap(a => State.unit(f(a)))
  def map[B](f: A => B): State[S, B] = monad.map(this)(f)

  //flatMap(a => sb.map(f(a, _)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = monad.map2(this, sb)(f)

  def get: State[S, S] = State(s => run(s) match { case (_, s1) => (s, s1)})

  def set(s: S): State[S, Unit] = State(_ => ((), s))

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

  def monad[S]: Monad[({type lambda[x] = State[S, x]})#lambda] =
    new Monad[({type lambda[x] = State[S, x]})#lambda] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = State(s => st.run(s) match {
          case (a, s1) => f(a).run(s1)
        })

    }

}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[A] = State[RNG, A]

  def simple(seed: Long): RNG = new RNG {
    override def toString: String = "RNG:simple:" + seed

    def nextInt: (Int, RNG) = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))

    }
  }

  val int: Rand[Int] = State(_.nextInt)

  val randomPair: Rand[(Int, Int)] = int.map2(int)((_, _))

  val positiveInt: Rand[Int] =
    int.flatMap(i => if (i != Int.MinValue) State.unit(i.abs) else positiveInt)

  val double: Rand[Double] = {
    positiveInt.map(_ / (Int.MaxValue.toDouble + 1))
  }

  val intDouble: Rand[(Int, Double)] = int.map2(double)((_, _))

  val doubleInt: Rand[(Double, Int)] =
    intDouble.map(p => (p._2, p._1))

  val double3: Rand[(Double, Double, Double)] =
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
