package monads

import parallelism.Par
import parallelism.Par.Par
import propertytesting.Gen
import state.State

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  override def apply[A, B](mf: M[A => B])(ma: M[A]): M[B] =
    flatMap(mf)(f => map[A, B](ma)(a => f(a)))

  override def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldLeft(unit(List.empty[A])) { (lm, m) =>
      flatMap(lm)(l => map(m)(_ :: l))
    }

  override def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List.empty[B])) { (lm, a) =>
      flatMap(lm)(l => map(f(a))(_ :: l))
    }

  override def replicateM[A](n: Int, ma: M[A]): M[List[A]] = n match {
    case 0 => map(ma)(_ => List.empty)
    case 1 => map(ma)(List(_))
    case i => flatMap(replicateM(i - 1, ma))(l => map(ma)(_ :: l))
  }

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  override def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(b) => map(b)(Right(_))
  }

  override def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def flatMapC[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)()

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  def flatMapJ[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def composeJ[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def applyM[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  def map[A,B](a: F[A])(f: A => B): F[B] = apply(unit(f))(a)

  def mapM[A,B](a: F[A])(f: A => B): F[B] = map2(unit(f), a)(_(_))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A])) {
      case (fs, fa) => map2(fs, map(fa)(_ :: Nil))(_ ++ _)
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def factor[A, B](fa: F[A], fb: F[A]): F[(A, B)] = ???

  def cofactor[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = ???

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = ???
}


case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}
// what is difference between Monad and ordinary trait?
// how it helps?
// why type parameter?
// why type lambda? how write explicitly?
// what is to lift?
object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonad[S]: Monad[State[S, _]] = new Monad[
    ({type lambda[x] = State[S, x]})#lambda] {

    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

}
