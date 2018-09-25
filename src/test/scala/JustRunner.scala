object JustRunner {
  def main(args: Array[String]): Unit = {
    import propertytesting2.Gen
    import state.RNG

    def run[A](seed: Int)(g: Gen[A]): A =
      g.sample.run(RNG.simple(seed))._1
    def run0[A] = run[A](0)(_)
    def run1[A] = run[A](4567890)(_)
    def run2[A] = run[A](-98765432)(_)
    def exh[A](g: Gen[A]) = g.exhaustive.toList

    run0(Gen.choose(-5, 11))
    run1(Gen.choose(-5, 11))
    run2(Gen.choose(-5, 11))
    exh(Gen.choose(-5, 11))

    exh(Gen.listOfN(0, Gen.choose(0, 1)))
    exh(Gen.listOfN(1, Gen.choose(0, 1)))
    exh(Gen.listOfN(1, Gen.choose(0, 2)))
    exh(Gen.listOfN(1, Gen.choose(0, 3)))
    exh(Gen.listOfN(2, Gen.choose(0, 1)))
    exh(Gen.listOfN(2, Gen.choose(0, 2)))
    exh(Gen.listOfN(3, Gen.choose(0, 1)))
    exh(Gen.listOfN(3, Gen.choose(0, 2)))
  }

}