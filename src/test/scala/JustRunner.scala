import parallelism.Par
import propertytesting.Gen.{choose, genIntFn, integer}
import propertytesting.Prop.{checkPar, forAllPar, run}
import propertytesting.{Gen, Prop}
import state.RNG

object JustRunner {
  val rng = RNG.simple(0)
  val rng1 = RNG.simple(1245341)
  val rng2 = RNG.simple(12431)
  val smallInt = choose(-10, 10)
  def main(args: Array[String]): Unit = {

    // PUT CODE HERE ...

  }

  def sample[A](g: Gen[A], l: RNG = rng) =
    (g.sample.run(l)._1)

}