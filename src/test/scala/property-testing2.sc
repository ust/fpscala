import lazyness.Stream
import propertytesting2.Gen
import state.RNG

def run[A](seed: Int)(g: Gen[A]): A =
  g.sample.run(RNG.simple(seed))._1
def run0[A] = run[A](0)(_)
def run1[A] = run[A](299)(_)
def run2[A] = run[A](98976432)(_)
def exh[A](g: Gen[A]): List[A] = g.exhaustive.flatMap(
  _.map(Stream(_)).getOrElse(Stream.empty)).toList

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

