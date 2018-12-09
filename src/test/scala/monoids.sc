import monoids.{Monoid, Stub, WC}
import propertytesting.Gen.**
import propertytesting.Prop.Result
import propertytesting.{Gen, Prop, SGen}
import state.RNG

val r0 = RNG.simple(0)
def run(p: Prop): Result = p.run(9, 300, r0)
Monoid.monoid[Map[List[String], Int => Map[String, List[Boolean]]]]

val wordsMonoid = Monoid.wordsMonoid("")
wordsMonoid.op("Hic", wordsMonoid.op("est ", "chorda ")) ==
  "Hic est chorda"
wordsMonoid.op("Hic ", wordsMonoid.op(" est", "chorda")) ==
  "Hic est chorda"
val strings = IndexedSeq("hui", "ne hui", "po huju", "hui")
Monoid.frequencyMap(strings)
Monoid.isOrdered(Vector())
Monoid.isOrdered(Vector(1, 1))
Monoid.isOrdered(Vector(-10, 0, 1, 10))
Monoid.isOrdered(Vector(-10, 0, -1, 10, 10))
Monoid.concatenate(List("abc", "1", "2", "3"))
Monoid.foldMap(List(999, 1, 2, 3))(_.toString)(Monoid.stringMonoid)
Monoid.foldMapV(Vector(999, 1, 2, 3))(_.toString)
Monoid.foldLeft(List(1, 2, 3))("hui")(_ + _)
Monoid.foldRight(List(1, 2, 3))("hui")(_.toString + _)
Monoid.splitCount("")
Monoid.splitCount(" ")
Monoid.splitCount("  ")
Monoid.splitCount("   ")
Monoid.splitCount("one")
Monoid.splitCount(" one two")
Monoid.splitCount(" one two ")
Monoid.splitCount("_one_same_one1")
Monoid.splitCount("one  two three ")
Monoid.splitCount("one  two  three ")
Monoid.splitCount("one  two  three 4")
Monoid.splitCount("one  two  three 4 5")

"-----------------------------------------------------------------"

def monoidLaws[A](g: SGen[A])(implicit m: Monoid[A]): Prop =
  Prop.forAll(g ** g ** g) { case a ** b ** c =>
    m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
  }

run(Prop.forAll(Gen.string)(str => {
  val m = Monoid.stringMonoid
  m.op(str, m.zero) == str
}))
val smallInt = Gen.choose(0, 4)
val eitherIntOrBool: Gen[Either[Boolean, Int]] =
  Gen.boolean.flatMap(b =>
    if (b) Gen.boolean.map(Left(_)) else smallInt.map(Right(_)))
implicit val product: Monoid[(Int, Boolean)] =
  Monoid.productMonoid(Monoid.intAddition, Monoid.booleanAnd)
implicit val coproduct: Monoid[Either[Boolean, Int]] =
  Monoid.coproductMonoid(Monoid.booleanAnd, Monoid.intAddition)
run(monoidLaws((smallInt ** Gen.boolean).unsized))
run(monoidLaws(eitherIntOrBool.unsized))
run(monoidLaws(Gen.choose(0, 4).unsized)(Monoid.intAddition))
run(monoidLaws(Gen.choose(0, 4).unsized)(Monoid.intMultiplication))
run(monoidLaws(Gen.boolean.unsized)(Monoid.booleanOr))
run(monoidLaws(Gen.boolean.unsized)(Monoid.booleanAnd))
val optGen = Gen.boolean.flatMap(b => Gen.choose(0, 3)
  .map(i => if (b) Some(i) else None))
run(monoidLaws[Option[Int]](optGen.unsized))
run(monoidLaws[WC](Gen.string.map(Stub)))
val genAtoA: Gen[String => String] =
  Gen.genStringIntFn(Gen.choose(0, 3)).map(f => f(_).toString)
run(Prop.forAll(genAtoA.unsized ** Gen.string) { case (fn, str) =>
  val m = Monoid.endoMonoid[String]
  m.op(fn, m.zero)(str) == fn(str)
})