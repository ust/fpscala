package datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

object Tree {

  def fold[A, B](t: Tree[A], z: (B, B) => B)(m: A => B): B =
    t match {
      case Leaf(a) => m(a)
      case Branch(l, r) => z(fold(l, z)(m), fold(r, z)(m))
    }

  def size[A](t: Tree[A]): Int =
    fold[A, Int](t, _ + _)(_ => 1)

  def maximum(t: Tree[Int]): Int =
    fold[Int, Int](t, _ max _)(x => x)

  def depth[A](t: Tree[A]): Int =
    fold[A, Int](t, (a, b) => 1 + (a max b))(_ => 1)

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t, (l, r) =>
      Branch[B](l, r))(x => Leaf(f(x)))
}
