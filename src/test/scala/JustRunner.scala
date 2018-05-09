import parallelism.Par
import propertytesting.Prop.{checkPar, forAllPar, run}
import propertytesting.{Gen, Prop}
import state.RNG

object JustRunner {
  def main(args: Array[String]): Unit = {

    val rng1 = RNG.simple(1245341)


    run(forAllPar(Gen.unit(1)) {
      i => Par.equal(Par.unit(2), Par.unit(2))
    }, rng = rng1)

//    Gen.unit(2).exhaustive
//    val pair = Gen.unit(1) ** Gen.unit(2)
//    pair.exhaustive
  }
}