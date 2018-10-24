import monoids.{Monoid, Stub, WC}
import propertytesting2.Prop.Result
import propertytesting2.{Gen, Prop, SGen}
import propertytesting2.Gen.**
import state.RNG

val r0 = RNG.simple(0)
def run(p: Prop): Result = p.run(9, 300, r0)

val wordsMonoid = Monoid.wordsMonoid("")
wordsMonoid.op("Hic", wordsMonoid.op("est ", "chorda ")) ==
  "Hic est chorda"
wordsMonoid.op("Hic ", wordsMonoid.op(" est", "chorda")) ==
  "Hic est chorda"

Monoid.concatenate(List("abc", "1", "2", "3"), Monoid.stringMonoid)
Monoid.foldMap(List(999, 1, 2, 3), Monoid.stringMonoid)(_.toString)
Monoid.foldLeft(List(1, 2, 3))("hui")(_ + _)
Monoid.foldRight(List(1, 2, 3))("hui")(_.toString + _)
Monoid.splitCount(" ")
Monoid.splitCount("one")
Monoid.splitCount(" one two")
Monoid.splitCount(" one two ")
Monoid.splitCount("one  two three ")
Monoid.splitCount("one  two  three ")

"-----------------------------------------------------------------"

def monoidLaws[A](m: Monoid[A])(g: SGen[A]): Prop =
  Prop.forAll(g ** g ** g) { case a ** b ** c =>
    m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
  }

run(Prop.forAll(Gen.string)(str => {
  val m = Monoid.stringMonoid
  m.op(str, m.zero) == str
}))
run(monoidLaws(Monoid.intAddition)(Gen.choose(0, 4).unsized))
run(monoidLaws(Monoid.intMultiplication)(Gen.choose(0, 4).unsized))
run(monoidLaws(Monoid.booleanOr)(Gen.boolean.unsized))
run(monoidLaws(Monoid.booleanAnd)(Gen.boolean.unsized))
run(monoidLaws[Option[Int]](Monoid.optionMonoid)(Gen.boolean
  .flatMap(b => Gen.choose(0, 3)
    .map(i => if (b) Some(i) else None)).unsized))
run(monoidLaws[WC](Monoid.wcMonoid)(Gen.string.map(Stub)))
val genAtoA: Gen[String => String] =
  Gen.genStringIntFn(Gen.choose(0, 3)).map(f => f(_).toString)
run(Prop.forAll(genAtoA.unsized ** Gen.string) { case (fn, str) =>
  val m = Monoid.endoMonoid[String]
  m.op(fn, m.zero)(str) == fn(str)
})