import lazyness.Stream
import propertytesting2.{Gen, Prop}
import state.RNG

val r0 = RNG.simple(0)
def run[A](seed: Int)(g: Gen[A]): A =
  g.sample.run(RNG.simple(seed))._1
def run0[A] = run[A](0)(_)
def run1[A] = run[A](299)(_)
def run2[A] = run[A](98976432)(_)
def exh[A](g: Gen[A]): List[A] = g.exhaustive.flatMap(
  _.map(Stream(_)).getOrElse(Stream.empty)).toList

val prop1 = Prop.forAll(Gen.boolean)(if (_) true else false)
prop1.run(3, r0)
val prop2 = Prop.forAll(Gen.choose(0, 3))(_ < 5)
prop2.run(5, r0)
prop2.run(100, r0)
(prop1 && prop2).run(100, r0)
(prop2 && prop1).run(100, r0)
(prop1 || prop2).run(100, r0)
(prop2 || prop1).run(100, r0)

"-------------------------------------------------------"

run0(Gen.union(Gen.choose(-4,-2), Gen.choose(2,4)))
run1(Gen.union(Gen.choose(-4,-2), Gen.choose(2,4)))
exh(Gen.union(Gen.choose(-4,-2), Gen.choose(2,4)))

run0(Gen.weighted((Gen.choose(-4, -2), 0.5), (Gen.choose(2, 4), 0.5)))
run1(Gen.weighted((Gen.choose(-4, -2), 0.5), (Gen.choose(2, 4), 0.5)))
exh(Gen.weighted((Gen.choose(-4, -2), 0.5), (Gen.choose(2, 4), 0.5)))

"-------------------------------------------------------"
run0(Gen.int)
run1(Gen.int)
run2(Gen.int)

run0(Gen.uniform)
run1(Gen.uniform)
run2(Gen.uniform)

run0(Gen.choose(9.5, 11.5))
run1(Gen.choose(9.5, 11.5))
run2(Gen.choose(9.5, 11.5))

run0(Gen.double)
run1(Gen.double)
run2(Gen.double)
"-------------------------------------------------------"
run0(Gen.choose(-5, 11))
run1(Gen.choose(-5, 11))
run2(Gen.choose(-5, 11))
exh(Gen.choose(-5, 11))
"zero"
exh(Gen.listOfN(0, Gen.choose(0, 1)))
exh(Gen.listOfN(0, Gen.choose(0, 2)))
exh(Gen.listOfN(0, Gen.choose(0, 3)))
"one"
exh(Gen.listOfN(1, Gen.choose(0, 1)))
exh(Gen.listOfN(1, Gen.choose(0, 2)))
exh(Gen.listOfN(1, Gen.choose(0, 3)))
"two"
exh(Gen.listOfN(2, Gen.choose(0, 1)))
exh(Gen.listOfN(2, Gen.choose(0, 2)))
exh(Gen.listOfN(2, Gen.choose(0, 3)))
"three"
exh(Gen.listOfN(3, Gen.choose(0, 1)))
exh(Gen.listOfN(3, Gen.choose(0, 2)))
exh(Gen.listOfN(3, Gen.choose(0, 3)))
"four"
exh(Gen.listOfN(4, Gen.choose(0, 1)))
exh(Gen.listOfN(4, Gen.choose(0, 2)))

