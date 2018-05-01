import propertytesting._
import state._

def print[A](s: State[RNG, A], l: RNG) = s.run(l)._1
def print[A](g: Gen[A], l: RNG) = g.sample.run(l)._1
def print[A](g: Gen[A]) =
  g.exhaustive.filter(_.isDefined).map(_.get).toList
val rng = RNG.simple(0)
val rng1 = RNG.simple(1245341)
val rng2 = RNG.simple(12431)

val smallInt = Gen.choose(-10, 10)
val assertListMax: List[Int] => Boolean = l => {
  val max = l.max
  !l.exists(_ > max)
}
val assertListSort: List[Int] => Boolean = l => {
  val ls = l.sorted
  l.isEmpty || !l.zip(l.tail).exists { case (a, b) => a > b }
}
Prop.run(Prop.forAll(Gen.listOf(smallInt))(assertListMax))
Prop.run(Prop.forAll(Gen.listOf1(smallInt))(assertListMax))
Prop.run(Prop.forAll(Gen.listOf(smallInt))(assertListSort),
  maxSize = 2, testCases = 1, rng = rng1)

val failedL = List(7, -3).sorted
failedL.zip(failedL.tail).exists { case (a, b) => a > b }

print(Gen.uniform)
print(Gen.uniform, rng)
print(Gen.uniform, rng1)
print(Gen.uniform, rng2)
print(Gen.choose(-12.2, 5.89))
print(Gen.choose(-12.2, 5.89), rng)
print(Gen.choose(-12.2, 5.89), rng1)
print(Gen.choose(-12.2, 5.89), rng2)
print(Gen.choose(-3, 5))
print(Gen.choose(-3, 5), rng1)
print(Gen.boolean)
print(Gen.boolean, rng)
print(Gen.boolean, rng1)
print(Gen.choose(1, 1).listOfN(2))
print(Gen.choose(1, 2).listOfN(2))
print(Gen.choose(1, 4).listOfN(1))
print(Gen.choose(1, 3).listOfN(2))
print(Gen.choose(1, 3).listOfN(3))
print(Gen.choose(1, 4).listOfN(2))
print(Gen.choose(1, 3).listOfN(0))
print(Gen.choose(1, 3).listOfN(2), rng1)
print(Gen.sameParity(1, 4))
print(Gen.randomListOf(Gen.choose(1, 6)), rng1)
print(Gen.randomListOf(Gen.character), rng1)
print(Gen.integer, rng2)
print(Gen.character, rng2)
print(Gen.short, rng2)
print(Gen.string, rng2)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 5)), rng)

print(Gen.weighted((Gen.choose(0, 5), 0.5),
  (Gen.choose(5, 10), 0.5)), rng2)
print(Gen.weighted((Gen.choose(0, 5), 0.3),
  (Gen.choose(5, 10), 0.7)), rng2)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 5)), rng1)
print(Gen.union(Gen.choose(0, 3), Gen.choose(3, 6)))



