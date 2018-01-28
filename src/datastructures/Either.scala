package datastructures

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(_) => this
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(f(a, _)))

  def traverse[E, A, B](a: scala.List[A])
                       (f: A => Either[E, B]): Either[E, scala.List[B]] =
    a.foldRight[Either[E, scala.List[B]]](Right(scala.Nil))((a, z) =>
      z.map2(f(a))((bs, b) => b :: bs))

  def sequence[E, A](a: scala.List[Either[E, A]]): Either[E, scala.List[A]] =
    traverse(a)(a => a)


}

