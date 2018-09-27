import lazyness.Stream

object JustRunner {
  def main(args: Array[String]): Unit = {
    import propertytesting2.Gen
    import state.RNG

    def run[A](seed: Int)(g: Gen[A]): A =
      g.sample.run(RNG.simple(seed))._1
    def run0[A] = run[A](0)(_)
    def run1[A] = run[A](2)(_)
    def run2[A] = run[A](9876432)(_)
    def exh[A](g: Gen[A]): List[A] = g.exhaustive.flatMap(
      _.map(Stream(_)).getOrElse(Stream.empty)).toList


    run0(Gen.uniform)
  }

}