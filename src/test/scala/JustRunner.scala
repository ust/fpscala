import lazyness.Stream
import propertytesting2.{Gen, Prop}
import propertytesting2.Prop.Result
import state.RNG

object JustRunner {
  def main(args: Array[String]): Unit = {

    val r0 = RNG.simple(0)
    def run[A](seed: Int)(g: Gen[A]): A =
      g.sample.run(RNG.simple(seed))._1
//    def run0[A](g: Gen[A]) = run[A](0)(g)
//    def run1[A](g: Gen[A]) = run[A](299)(g)
    def run2[A](g: Gen[A]) = run[A](98976432)(g)
    def exh[A](g: Gen[A]): List[A] = g.exhaustive.flatMap(
      _.map(Stream(_)).getOrElse(Stream.empty)).toList
    def run0(p: Prop): Result = p.run(100, 5, r0)
    def run1(p: Prop): Result = p.run(100, 100, r0)


    val prop1 = Prop.forAll(Gen.boolean)(if (_) true else false)
    val prop2 = Prop.forAll(Gen.choose(0, 3))(_ < 5)

    //run1(prop1)
    run0(prop2)
  }

}