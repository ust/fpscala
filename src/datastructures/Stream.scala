package datastructures

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
  def startsWith[A](that: Stream[A]): Boolean =
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
      case scala.Some((a, as)) => scala.Some(s, as)
      case _ => scala.None
    }).append(Stream.empty)

  def hasSubsequence[A](s1: Stream[A],
                        s2: Stream[A]): Boolean =
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
