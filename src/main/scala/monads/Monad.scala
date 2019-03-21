package monads

import java.text.SimpleDateFormat
import java.util.Date

import parallelism.Par
import parallelism.Par.Par
import propertytesting.Gen
import state.State

import scala.annotation.tailrec
import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def applyM[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab(a))

  def unit[A](a: => A): F[A]

  override def map[A,B](a: F[A])(f: A => B): F[B] = apply(unit(f))(a)

  def mapM[A,B](a: F[A])(f: A => B): F[B] = map2(unit(f), a)(_(_))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A]))(map2(_, _) { case (l, a) => a :: l })

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    @tailrec
    def rpl(acc: F[List[A]], fa: F[A], n: Int): F[List[A]] = n match {
      case x if x < 0 => throw new IllegalArgumentException("n shouldn't be negative")
      case 0          => acc
      case x          => rpl(map2(fa, acc)(_ :: _), fa, x - 1)
    }
    rpl(unit(Nil), fa, n)
  }

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def cofactor[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(b) => map(b)(Right(_))
  }
}

trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]


  override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

  override def apply[A, B](mf: M[A => B])(ma: M[A]): M[B] =
    flatMap(mf)(f => map[A, B](ma)(a => f(a)))

  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def flatMapC[A, B](ma: M[A])(f: A => M[B]): M[B] = compose((_: Unit) => ma, f)()

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  def flatMapJ[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def composeJ[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

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

  def stateMonad[S]: Monad[({type lambda[x] = State[S, x]})#lambda] =
    new Monad[({type lambda[x] = State[S, x]})#lambda] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))

      def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ma flatMap f
    }
}

object Applicative {
  def validationApplicative[E]: Applicative[({type v[x] = Validation[E, x]})#v] =
    new Applicative[({type v[x] = Validation[E, x]})#v] {
      def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        (fab, fa) match {
          case (Success(ab),   Success(a)     ) => Success(ab(a))
          case (Failure(h, t), Failure(h0, t0)) => Failure[E](h0, (t0 :+ h) ++ t)
          case (f: Failure[E], _              ) => f
          case (_,             f: Failure[E]  ) => f
          case _                                => throw new IllegalArgumentException
        }

      def unit[A](a: => A): Validation[E, A] = Success(a)
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthDate: Date, phoneNumber: String)

object WebForm {
  def validName(name: String): Validation[String, String] =
    if (name != "")
      Success(name)
    else Failure("Name cannot be empty")

  def validBirthDate(birthDate: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthDate))
    } catch {
      case _: Throwable => Failure("birthDate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                   birthDate: String,
                   phone: String): Validation[String, WebForm] = {
    val a: Applicative[({type v[x] = Validation[String, x]})#v] = Applicative.validationApplicative[String]
    a.apply(a.apply(a.apply(a.unit((WebForm(_, _, _)).curried))(
      validName(name)))(
      validBirthDate(birthDate)))(
      validPhone(phone))
  }

}