import monads._
import propertytesting.Gen._
import propertytesting.Prop.Result
import propertytesting.{Gen, Prop, SGen}
import state.RNG

import scala.language.higherKinds

val r0 = RNG.simple(0)
def run0(p: Prop): Result = p.run(10, 500, r0)

val genOptInt: Gen[Option[Int]] = (Gen.int ** Gen.boolean)
  .map { case i ** b => if (b) Some(i) else None }
val genFnIntInt: Gen[Int => Option[Int]] =
  Gen.int.map { i => if (i % 2 == 1) Some(_) else _ => None }
val genFnIntStr: Gen[Int => Option[String]] =
  Gen.boolean.map {
    case true => i => Some(i.toString)
    case _    => _ => None
  }
val genFnStrInt: Gen[String => Option[Int]] =
  Gen.boolean.map {
    case true => s => Some(s.length)
    case _    => _ => None
  }
val genFnFnMonad = for {
  a <- genFnIntStr
  b <- genFnStrInt
  c <- Gen.int
  h <- genOptInt
  g <- genOptInt
  x <- genOptInt
} yield (a, b, c, h.map(_ => a), g.map(_ => b), x.map(_ => x))

type ValidationString[+A] = Validation[String, A]
val genStrStrAppl: Gen[Validation[String, String]] = (Gen.boolean ** Gen.character).map {
  case (true, b) => Success("" + b + b)
  case (_,    b) => Failure("" + b + b)
}
val valStrApplicative: Applicative[ValidationString] = Applicative.validationApplicative[String]

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

def lawCompositionApply[A, B, C, F[_]](gen: SGen[(A => B, B => C, A, F[A => B], F[B => C], F[A])])
                                      (f: Applicative[F]) = Prop.forAll(gen) {
  case (a, b, c, g, h, x) =>
    //f.apply(f.apply[A => B, A => C](f.unit(ab => bc => c0 => bc(ab(c0))))(g))(x) ==
      //f.apply(h)(f.apply(g)(x))
  true
}


run0(lawAssociativeFlatMap((genOptInt ** genFnIntInt ** genFnIntInt).unsized)(Monad.optionMonad))
run0(lawAssociativeCompose((genFnIntStr ** genFnStrInt ** genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawIdentityCompose((genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawIdentityFlatMap((genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawIdentityJoin((genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawAssociativeJoin((genFnIntStr ** genFnStrInt ** genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawIdentityMap[String, ValidationString](genStrStrAppl.unsized)(valStrApplicative))
run0(lawIdentityApply[String, ValidationString](genStrStrAppl.unsized)(valStrApplicative))

