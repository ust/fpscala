package datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def tail[A](xs: List[A]): List[A] = drop(xs, 1)

  def drop[A](xs: List[A], n: Int): List[A] =
    if (n == 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail)(f) else l
    }

  def setHead[A](l: List[A], a: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => Cons(a, tail)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(a, Cons(_, Nil)) => Cons(a, Nil)
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l: List[A]): Int =
    foldLeft[A, Int](l, 0)((x, _) => x + 1)

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prod(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((z, a) => Cons(a, z))

  def append[A](l: List[A], a: A) =
    foldRight(l, Cons(a, Nil))(Cons(_, _))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    // reverse
    val rev = foldRight(l, Nil: List[A])((a, as) => append(as, a))
    // apply f
    foldRight[A, B](rev, z)((a, b) => f(b, a))
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((a, b) => f(b, a))

  def flatten[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, Nil: List[A])((a, b) =>
      foldRight(a, b)(Cons(_, _)))

  def map[A, B](l: List[A])(m: A => B): List[B] =
    foldRight(l, Nil: List[B])((a, b) => Cons(m(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    reverse(foldLeft(l, Nil: List[B])((b, a) =>
      foldLeft(f(a), b)((b, a) => Cons(a, b))))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def join[A](l: List[A], r: List[A])(f: (A, A) => A): List[A] =
    l match {
      case Nil => r
      case Cons(lh, lt) => r match {
        case Nil => l
        case Cons(rh, rt) =>
          Cons(f(lh, rh), join(lt, rt)(f))
      }
    }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def go(a: List[A], b: List[A]): Boolean =
      a match {
        case Nil => if (b == Nil) true else false
        case Cons(ah, at) => b match {
          case Nil => true
          case Cons(bh, bt) =>
            if (ah == bh) go(at, bt)
            else go(at, sub)
        }
      }

    go(l, sub)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}