package monoids

import datastructures.Tree

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object Monoid {

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }

  def coproductMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[Either[A, B]] =
    new Monoid[Either[A, B]] {
      override def op(a1: Either[A, B], a2: Either[A, B]): Either[A, B] = (a1, a2) match {
        case (Left(x),  Left(y))  => Left(A.op(x, y))
        case (Left(x),  _)        => Left(x)
        case (Right(x), Right(y)) => Right(B.op(x, y))
        case (_,        Left(y))  => Left(y)
      }

      override def zero: Either[A, B] = Left(A.zero)
    }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(a), Stub(b)) => cons(a + b)
      case (Stub(l), Part(m, n, r)) => op(cons(s"$l$m "), Part("", n, r))
      case (Part(l, n, m), Stub(r)) => op(Part(l, n, ""), cons(s" $m$r"))
      case (Part(l, n1, m1), Part(m2, n2, r)) =>
        Part(l, n1 + s"$m1$m2".oneOrZero + n2, r)
    }

    private def cons(str: String): WC = str.foldLeft[WC](Stub("")) {
      case (Stub(s),        ' ' ) => Part(s, 0, "")
      case (Stub(s),        c   ) => Stub(s + c)
      case (Part(l, n, r),  ' ' ) => Part(l, n + r.oneOrZero, "")
      case (Part(l, n, r),  c   ) => Part(l, n, r + c)
    }

    def zero: WC = Part("", 0, "")
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2

    def zero: A => A = identity
  }

  def wordsMonoid(s: String): Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1.trim + " " + a2.trim

    def zero: String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def splitCount(s: String, chunkSize: Int = 10): Int = {
    def go(chunk: String): WC = if (chunk.length > chunkSize)
      wcMonoid.op(go(chunk.substring(0, chunkSize)),
        go(chunk.substring(chunkSize)))
    else Stub(chunk)

    wcMonoid.op(wcMonoid.zero, wcMonoid.op(go(s), wcMonoid.zero)) match {
      case Part(l, n, r) => l.oneOrZero + n + r.oneOrZero
      case _ => 1
    }
  }


  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val l = v.length
    if (l > 2) {
      val chunks = v.splitAt(l / 2)
      m.op(foldMapV(chunks._1, m)(f), foldMapV(chunks._2, m)(f))
    } else l match {
      case 0 => m.zero
      case 1 => f(v(0))
      case 2 => m.op(f(v(0)), f(v(1)))
    }
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid[B])(a => f(_, a))(z)

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(a => f(a, _))(z)

  def isOrdered(v: IndexedSeq[Int]): Boolean =
    foldMapV(v, new Monoid[Int => Option[Int]] {
      def op(a1: Int => Option[Int], a2: Int => Option[Int]): Int => Option[Int] =
        i1 => a1(i1).flatMap(i2 => a2(i2))

      def zero: Int => Option[Int] = Some(_)
    })(i1 => i2 => if (i1 >= i2) Some(i1) else None)(Int.MinValue).nonEmpty

  implicit class StringOneOrZero(string: String) {
    def oneOrZero: Int = string.headOption.fold(0)(_ => 1)
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldMap(fa)(List(_))(new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    def zero: List[A] = List.empty
  })
}

class FoldableList extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)

  override def toList[A](fa: List[A]): List[A] = fa
}

class FoldableIndexedSeq extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMapV(as, mb)(f)
}

class FoldableStream extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b: B, a: A) => mb.op(b, f(a)))
}

class FoldableTree extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    Tree.fold[A, B => B](as, _ andThen _)(a => f(a, _))(z)

  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
    Tree.fold[A, B => B](as, _ andThen _)(a => f(_, a))(z)

  def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    Tree.fold(as, mb.op)(f)
}

class FoldableOption extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as.map(f(_, z)).getOrElse(z)

  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.map(f(z, _)).getOrElse(z)

  def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).getOrElse(mb.zero)
}