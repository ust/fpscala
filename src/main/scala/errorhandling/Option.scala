package errorhandling

import java.util.regex.{Pattern, PatternSyntaxException}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(a => f(a)).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(_ => this).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) this else None)
}

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(ds: Seq[Double]): Option[Double] =
      ds.foldLeft(0)((z, _) => z + 1) match {
        case 0 => None
        case c => Some(ds.foldLeft(0.0)(_ + _) / c)
      }

    mean(xs).flatMap(m =>
      mean(xs.map(x => math.pow(x - m, 2))))
  }

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def map2[A, B, C](a: Option[A],
                    b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(g => b.map(h => f(g, h)))

  def bothMatch_2(pat1: String,
                  pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((a, b) =>
      a(s) && b(s))

  def traverse[A, B](a: scala.List[A])
                    (f: A => Option[B]): Option[scala.List[B]] =
    a.foldRight[Option[scala.List[B]]](Some(scala.Nil))((a, z) =>
      map2(z, f(a))((bs, b) => b :: bs))

  def sequence[A](a: scala.List[Option[A]]): Option[scala.List[A]] =
    traverse(a)(a => a)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]
