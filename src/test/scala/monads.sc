import monads.Monad
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
    case _ => _ => None
  }
val genFnStrInt: Gen[String => Option[Int]] =
  Gen.boolean.map {
    case true => s => Some(s.length)
    case _ => _ => None
  }
def lawFlatMap[A, M[_]](gen: SGen[((M[A], A => M[A]), A => M[A])])
                  (m: Monad[M]) =
  Prop.forAll(gen) {
    case ((x, f), g) =>
      m.flatMap(m.flatMap(x)(f))(g) == m.flatMap(x)(a => m.flatMap(f(a))(g))
  }
def lawCompose[A, B, C, D, M[_]](gen: SGen[(((A => M[B], B => M[C]), C => M[D]), A)])
                           (m: Monad[M]) =
  Prop.forAll(gen) {
    case (((f, g), h), i) =>
      m.compose(m.compose(f, g), h)(i) == m.compose(f, m.compose(g, h))(i)
  }
def lawComposeUnit[A, B, C, D, M[_]](gen: SGen[(A => M[B], A)])
                                    (m: Monad[M]) =
  Prop.forAll(gen) { case (f, a) =>
    m.compose(m.unit[A], f)(a) == f(a)
  } && Prop.forAll(gen) { case (f, a) =>
    m.compose(f, (x: B) => m.unit[B](x))(a) == f(a)
  }

run0(lawFlatMap((genOptInt ** genFnIntInt ** genFnIntInt).unsized)(Monad.optionMonad))
run0(lawCompose((genFnIntStr ** genFnStrInt ** genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))
run0(lawComposeUnit((genFnIntStr ** Gen.int).unsized)(Monad.optionMonad))

