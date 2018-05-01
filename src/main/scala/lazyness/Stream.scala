package lazyness

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case None => Nil
    case Some((h, t)) => h :: t.toList
  }

  def take(n: Int): Stream[A] =
    Stream.unfold((n, this))(s =>
      if (s._1 < 1) None else s._2.uncons match {
        case Some((a, as)) =>
          Some((a, (s._1 - 1, as)))
        case None => None
      })

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this)(s => s.uncons match {
      case Some((h, t)) if p(h) => Some((h, t))
      case _ => None
    })

  def map[B](f: A => B): Stream[B] =
    Stream.unfold(this)(s => s.uncons match {
      case Some((a, as)) => Some((f(a), as))
      case None => None
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
      case Some((a, as)) => s._2.uncons match {
        case Some((b, bs)) => Some(((a, b), (as, bs)))
        case None => None
      }
      case None => None
    })

  def zipAll[A1 >: A, B](that: Stream[B],
                         a0: A1, b0: B): Stream[(A1, B)] =
    Stream.unfold((this, that))(s => s._1.uncons match {
      case Some((a, as)) =>
        s._2.uncons match {
          case Some((b, bs)) =>
            Some(((a, b), (as, bs)))
          case None =>
            Some(((a, b0), (as, Stream.empty)))
        }
      case None => s._2.uncons match {
        case Some((b, bs)) =>
          Some(((a0, b), (Stream.empty, bs)))
        case None => None
      }
    })

  // todo with zip and forAll
  def startsWith[A1 >: A](that: Stream[A1]): Boolean =
    Stream.unfold((this, that))(s => s._2.uncons match {
      case Some((h, t)) => s._1.uncons match {
        case Some((a, as)) =>
          if (h == a)
            Some(false, (as, t)) else None
        case _ => None
      }
      case _ => Some(true, (Stream.empty, Stream.empty))
    }).exists(_ == true)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(s => s.uncons match {
      case Some((_, as)) => Some(s, as)
      case _ => None
    }).append(Stream.empty)

  def hasSubsequence[A1 >: A](s1: Stream[A1],
                              s2: Stream[A1]): Boolean =
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
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.unfold(a)(_ => Some((a, a)))

  def from(n: Int): Stream[Int] =
    Stream.unfold(n)(a => Some(a, a + 1))

  def fibs: Stream[Int] =
    Stream.unfold((0, 1))(s =>
      Some((s._1, (s._2, s._1 + s._2))))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map(p => Stream.cons(p._1, unfold(p._2)(f)))
      .getOrElse(Stream.empty)

}