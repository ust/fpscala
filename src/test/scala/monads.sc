import monads._
import propertytesting.Gen._
import propertytesting.Prop.Result
import propertytesting.{Gen, Prop, SGen}
import state.RNG
import Monad._
import Gen._
import scala.language.higherKinds

val r0 = RNG.simple(0)

def run0(p: Prop): Result = p.run(10, 500, r0)

val genOptInt: Gen[Option[Int]] = (int ** boolean)
  .map { case i ** b => if (b) Some(i) else None }
val genFnStr2Int: Gen[String => Int] = int.map(i => _.hashCode * i)
val genFnInt2Str: Gen[Int => String] = character.map(c => c + _.toString)
val genFnInt2OptInt: Gen[Int => Option[Int]] = int.map { i => if (i % 2 == 1) Some(_) else _ => None }
val genFnInt2OptStr: Gen[Int => Option[String]] =
  boolean.map {
    case true => i => Some(i.toString)
    case _ => _ => None
  }
val genFnStr2OptInt: Gen[String => Option[Int]] =
  boolean.map {
    case true => s => Some(s.length)
    case _ => _ => None
  }
val genOptFnInt2Str: Gen[Option[Int => String]] = genOptInt.map2(genFnInt2Str)((o, f) => o.map(_ => f))
val genAbBcAMonad = for {
  a <- genFnInt2Str
  b <- genFnStr2Int
  c <- Gen.int
  h <- genOptInt
  g <- genOptInt
  x <- genOptInt
} yield (h.map(_ => a), g.map(_ => b), x)

val genStrStrAppl: Gen[Validation[String, String]] = (boolean ** character).map {
  case (true, b) => Success("" + b + b)
  case (_, b) => Failure("" + b + b)
}
type ValidationString[+A] = Validation[String, A]
//val valStrApplicative: Applicative[ValidationString] = Validation. applicative[String]

def lawAssociativeFlatMap[A, M[_]](gen: SGen[((M[A], A => M[A]), A => M[A])])
                                  (m: Monad[M]) =
  Prop.forAll(gen) {
    case ((x, f), g) =>
      m.flatMap(m.flatMap(x)(f))(g) == m.flatMap(x)(a => m.flatMap(f(a))(g))
  }

def lawAssociativeCompose[A, B, C, D, M[_]](gen: SGen[(((A => M[B], B => M[C]), C => M[D]), A)])
                                           (m: Monad[M]) =
  Prop.forAll(gen) {
    case (((f, g), h), i) =>
      m.compose(m.compose(f, g), h)(i) == m.compose(f, m.compose(g, h))(i)
  }

def lawIdentityCompose[A, B, C, D, M[_]](gen: SGen[(A => M[B], A)])
                                        (m: Monad[M]) =
  Prop.forAll(gen) { case (f, a) =>
    val func1: A => M[B] = m.compose(m.unit[A](_), f)
    val func2: A => M[B] = m.compose(f, (x: B) => m.unit[B](x))
    func1(a) == f(a) && func1(a) == func2(a)
  }

def lawIdentityFlatMap[A, B, C, D, M[_]](gen: SGen[(A => M[B], A)])
                                        (m: Monad[M]) =
  Prop.forAll(gen) { case (f, a) =>
    val func1: A => M[B] = x => m.flatMap(f(x))(m.unit[B](_))
    val func2: A => M[B] = x => m.flatMap(m.unit(x))(f)
    func1(a) == func2(a) && func1(a) == f(a)
  }

def lawIdentityJoin[A, B, C, D, M[_]](gen: SGen[(A => M[B], A)])
                                     (m: Monad[M]) =
  Prop.forAll(gen) { case (f, a) =>
    val func1: A => M[B] = x => m.join(m.map(f(x))(m.unit[B](_)))
    val func2: A => M[B] = x => m.join(m.map(m.unit[A](x))(f))
    func1(a) == func2(a) && func1(a) == f(a)
  }

def lawAssociativeJoin[A, B, C, D, M[_]](gen: SGen[(((A => M[B], B => M[C]), C => M[D]), A)])
                                        (m: Monad[M]) =
  Prop.forAll(gen) {
    case (((f, g), h), i) =>
      val func1: A => M[D] = x => m.join(m.map(m.join(m.map(f(x))(g)))(h))
      val func2: A => M[D] = x => m.join(m.map(f(x))(y => m.join(m.map(g(y))(h))))
      func1(i) == func2(i)
  }

def lawIdentityMap[A, F[_]](gen: SGen[F[A]])(f: Applicative[F]) =
  Prop.forAll(gen)(functor => f.map(functor)(x => x) == functor)

def lawIdentityApply[A, F[_]](gen: SGen[F[A]])(f: Applicative[F]) =
  Prop.forAll(gen)(functor => f.apply(f.unit[A => A](identity))(functor) == functor)

def lawCompositionApply[A, B, C, F[_]](gen: SGen[(F[A => B], F[B => C], F[A])])
                                      (f: Applicative[F]) = Prop.forAll(gen) {
  case (g, h, x) =>
    f.apply[A, C](
      f.apply[A => B, A => C](
        f.apply[B => C, (A => B) => A => C](
          f.unit(bc => ab => a0 => bc(ab(a0)))
        )(h)
      )(g)
    )(x) ==
      f.apply(h)(f.apply(g)(x))

}

def lawCompositionMap2[A, B, C, F[_]](gen: SGen[(F[A => B], F[B => C], F[A])])
                                     (f: Applicative[F]) = Prop.forAll(gen) {
  case (g, h, x) => f.apply(f.map2(h, g)(_ compose _))(x) == f.apply(h)(f.apply(g)(x))
}

def lawCompositionMap3[A, B, C, F[_]](gen: SGen[(F[A => B], F[B => C], F[A])])
                                     (f: Applicative[F]) = Prop.forAll(gen) {
  case (g, h, x) => f.map3(h, g, x)((a, b, c) => a(b(c))) == f.apply(h)(f.apply(g)(x))
}

def lawHomomorphismUnit[A, B, F[_]](gen: SGen[(A => B, A)])
                                   (f: Applicative[F]) = Prop.forAll(gen) {
  case (g, a) =>
    val v0 = f.unit(g(a))
    f.apply(f.unit(g))(f.unit(a)) == v0 && f.map(f.unit(a))(g) == v0
}

def lawInterchangeUnit[A, B, F[_]](gen: SGen[(F[A => B], A)])
                                  (f: Applicative[F]) = Prop.forAll(gen) {
  case (g, a) =>
    f.apply(g)(f.unit(a)) == f.apply[A => B, B](f.unit(_ (a)))(g)
}

def lawCompose[A, F[_], G[_]](gen: SGen[A])
                             (f: Applicative[F], g: Applicative[G]) =
  Prop.forAll(gen)(a => (g compose f).unit(a) == g.unit(f.unit(a)))


run0(lawAssociativeFlatMap((genOptInt ** genFnInt2OptInt ** genFnInt2OptInt).unsized)(optionMonad))
run0(lawAssociativeCompose((genFnInt2OptStr ** genFnStr2OptInt ** genFnInt2OptStr ** Gen.int).unsized)(optionMonad))
run0(lawIdentityCompose((genFnInt2OptStr ** Gen.int).unsized)(optionMonad))
run0(lawIdentityFlatMap((genFnInt2OptStr ** Gen.int).unsized)(optionMonad))
run0(lawIdentityJoin((genFnInt2OptStr ** Gen.int).unsized)(optionMonad))
run0(lawAssociativeJoin((genFnInt2OptStr ** genFnStr2OptInt ** genFnInt2OptStr ** Gen.int).unsized)(optionMonad))
//run0(lawIdentityMap[String, ValidationString](genStrStrAppl.unsized)(valStrApplicative))
//run0(lawIdentityApply[String, ValidationString](genStrStrAppl.unsized)(valStrApplicative))
run0(lawCompositionApply(genAbBcAMonad.unsized)(optionMonad))
run0(lawCompositionMap2(genAbBcAMonad.unsized)(optionMonad))
run0(lawCompositionMap3(genAbBcAMonad.unsized)(optionMonad))
run0(lawHomomorphismUnit((genFnInt2Str ** Gen.int).unsized)(optionMonad))
run0(lawInterchangeUnit((genOptFnInt2Str ** Gen.int).unsized)(optionMonad))
run0(lawCompose(Gen.int.unsized)(listMonad, optionMonad))
