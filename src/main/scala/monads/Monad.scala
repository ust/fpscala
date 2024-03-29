package monads

import java.text.SimpleDateFormat
import java.util.Date

import monads.Applicative.Const
import monoids.{Foldable, Monoid}
import parallelism.Par
import parallelism.Par.Par
import propertytesting.Gen
import state.State

import scala.annotation.tailrec

object Functor {
  type Idx[A] = A
}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}


trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] = sequence(map(fa)(f))

  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] = traverse(fma)(ma => ma)

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Functor.Idx, A, B](fa)(f)(Monad.idxMonad)

  def foldMap[A, B](as: F[A])(f: A => B)(implicit mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, B](as)(f)(mb) // B -> Nothing

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(State.monad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa) { a =>
    for {
      s       <- State.get[S]
      (b, s1)  = f(a, s)
      _       <- State.set(s1)
    } yield b
  }.run(s)

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] = mapAccum(fa, toList(fb)) { (a, s) => ((a, s.head), s.tail) }._1

  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, s) => ((), f(s, a)))._2

  override def toList[A](fa: F[A]): List[A] = mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, ())((a, _) => (a, ()))._1

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def fuse[M[_], N[_], A, B](fa: F[A])
                            (f: A => M[B], g: A => N[B])
                            (M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) = {
    type P[x] = (M[x], N[x])
    traverse[P, A, B](fa)(a => (f(a), g(a)))(M product N)
  }

//  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
//    new Traverse[({type f[x] = F[G[x]]})#f] {
//      override def sequence[M[_] : Applicative, A](fma: F[G[M[A]]]): M[F[G[A]]] = self.sequence(fma)
//    }

}

object Traverse {
  val listTraverse: Monad[List] = Monad.listMonad

  val optionTraverse: Monad[Option] = Monad.optionMonad

  val treeTraverse: Monad[Tree] = new Monad[Tree] {
    def unit[A](a: => A): Tree[A] = Tree(a, List.empty)

    def flatMap[A, B](ma: Tree[A])(f: A => Tree[B]): Tree[B] = ma match {
      case Tree(a, mas) => f(a) match {
        case Tree(b, mbs) => Tree(b, mas.map(flatMap(_)(f)) ++ mbs)
      }
    }
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Applicative[F[_]] extends Functor[F] {
  self =>

  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def applyM2[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_ (_))

  override def map[A, B](a: F[A])(f: A => B): F[B] = apply(unit(f))(a)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def mapM[A, B](a: F[A])(f: A => B): F[B] = map2(unit(f), a)(_ (_))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(List.empty[A]))(map2(_, _) { case (l, a) => a :: l })

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V])) {
    case (fm, (k, fv)) => apply[V, Map[K, V]](map(fm)(mv => v => mv + (k -> v)))(fv)
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    sequence(as.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    @tailrec
    def rpl(acc: F[List[A]], fa: F[A], n: Int): F[List[A]] = n match {
      case x if x < 0 => throw new IllegalArgumentException("n shouldn't be negative")
      case 0 => acc
      case x => rpl(map2(fa, acc)(_ :: _), fa, x - 1)
    }

    rpl(unit(Nil), fa, n)
  }

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def cofactor[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(b) => map(b)(Right(_))
  }

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

      def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
        self.apply(self.map[G[A => B], G[A] => G[B]](fab)(gab => G.apply(gab)(_)))(fa)
    }
}

object Applicative {
  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      def apply[A, B](fab: M)(fa: M): M = M.op(fab, fa)
    }
}

trait Monad[M[_]] extends Applicative[M] {
  self =>

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

  def compose[N[_]](N: Monad[N]): Monad[({type f[x] = M[N[x]]})#f] =
    new Monad[({type f[x] = M[N[x]]})#f] {
      def unit[A](a: => A): M[N[A]] = self.unit(N.unit(a))

      def flatMap[A, B](ma: M[N[A]])(f: A => M[N[B]]): M[N[B]] =
        //self.flatMap(ma)(na => self.unit(N.flatMap(na)(f(_))))
        self.apply(self.map[N[A => M[N[B]]], N[A] => N[B]](self.unit(N.unit(f)))(_ => ???))(ma)
    }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Monad {
  import Functor._

//  def composeM[M[_],N[_]](M: Monad[M], N: Monad[N], T: Traverse[N]): Monad[({type f[x] = M[N[x]]})#f] =
//    new Monad[({type f[x] = M[N[x]]})#f] {
//      override def unit[A](a: => A): M[N[A]] = M.unit(N.unit(a))
//
//      override def flatMap[A, B](ma: M[N[A]])(f: A => M[N[B]]): M[N[B]] =
//        M.flatMap(ma)(na =>
//          M.map(
//            T.traverse(na)(a =>
//              M.flatMap(f(a))(nb =>
//                T.traverse(nb)(
//                  M.unit
//                )(M)
//              )
//            )(M)
//          )(N.join)
//        )
//    }

  val genMonad: Monad[Gen] = Gen.monad

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
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  val idxMonad: Monad[Idx] = new Monad[Idx] {
    def unit[A](a: => A): Idx[A] = a

    def flatMap[A, B](ma: Idx[A])(f: A => Idx[B]): Idx[B] = f(ma)
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = State.monad

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ma flatMap f
    }
}

object Validation {
  def applicative[E]: Applicative[({type v[x] = Validation[E, x]})#v] =
    new Applicative[({type v[x] = Validation[E, x]})#v] {
      def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        (fab, fa) match {
          case (Success(ab),   Success(a)     ) => Success(ab(a))
          case (Failure(h, t), Failure(h0, t0)) => Failure[E](h0, (t0 :+ h) ++ t)
          case (f: Failure[E], _              ) => f
          case (_,             f: Failure[E]  ) => f
        }

      def unit[A](a: => A): Validation[E, A] = Success(a)
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthDate0: Date, phoneNumber: String)

object WebForm {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthDate(birthDate1: String): Validation[String, Date] =
    try {
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthDate1))
    } catch {
      case _: Throwable => Failure("birthDate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                   birthDate: String,
                   phone: String): Validation[String, WebForm] = {
    val a: Applicative[({type v[x] = Validation[String, x]})#v] = Validation.applicative[String]
    a.apply(a.apply(a.apply(a.unit((WebForm(_, _, _)).curried))(
      validName(name)))(
      validBirthDate(birthDate)))(
      validPhone(phone))
  }

}