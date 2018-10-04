import monoids.Monoid
import propertytesting2.Prop.Result
import propertytesting2.{Gen, Prop}
import state.RNG

val r0 = RNG.simple(0)
def run(p: Prop): Result = p.run(9, 300, r0)

val wordsMonoid = Monoid.wordsMonoid("")
wordsMonoid.op("Hic", wordsMonoid.op("est ", "chorda ")) ==
  "Hic est chorda"
wordsMonoid.op("Hic ", wordsMonoid.op(" est", "chorda")) ==
  "Hic est chorda"

"-----------------------------------------------------------------"

def monoidLaws[A](m: Monoid[A])(g: Gen[A]): Prop =
  Prop.forAll(g)(a => m.op(a, m.zero) == a)
//Prop.forAll(g)(a => m.op(m.zero, a) == a)
def monoidLawsFn[A](m: Monoid[A])(g: Gen[A]): Prop =
  Prop.forAll(g)(a => m.op(a, m.zero) == a)

run(Prop.forAll(Gen.string)(str => {
  val m = Monoid.stringMonoid
  m.op(str, m.zero) == str
}))
run(monoidLaws(Monoid.intAddition)(Gen.choose(0, 4)))
run(monoidLaws(Monoid.intMultiplication)(Gen.choose(0, 4)))
run(monoidLaws(Monoid.booleanOr)(Gen.boolean))
run(monoidLaws(Monoid.booleanAnd)(Gen.boolean))
run(monoidLaws[Option[Int]](Monoid.optionMonoid)(Gen.boolean
  .flatMap(b => Gen.choose(0, 3)
    .map(i => if (b) Some(i) else None))))
val genAtoA: Gen[String => String] =
  Gen.genStringIntFn(Gen.choose(0, 3)).map(f => f(_).toString)
run(Prop.forAll(genAtoA.unsized ** Gen.string) { case (fn, str) =>
  val m = Monoid.endoMonoid[String]
  m.op(fn, m.zero)(str) == fn(str)
})