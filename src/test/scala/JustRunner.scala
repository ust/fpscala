import propertytesting.{Gen, Prop}
import state.RNG

object JustRunner {
  def main(args: Array[String]): Unit = {

    val rng1 = RNG.simple(1245341)

    val smallInt = Gen.choose(-10, 10)

    val assertListSort: List[Int] => Boolean = l => {
      val ls = l.sorted
      l.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
    }


    Prop.run(Prop.forAll(Gen.listOf(smallInt))(assertListSort),
      maxSize = 2, testCases = 1, rng = rng1)


  }
}