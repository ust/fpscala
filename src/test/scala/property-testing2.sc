import propertytesting2.Gen
import state.RNG

def run[A](seed: Int)(g: Gen.Gen[A]): A =
  g.run(RNG.simple(seed))._1

def run0[A] = run[A](0)(_)
def run1[A] = run[A](1000)(_)
def run2[A] = run[A](-1000)(_)


val g = Gen.choose(-5, 11)

run0(g)
run1(g)
run2(g)