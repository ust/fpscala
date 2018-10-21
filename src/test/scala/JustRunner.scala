import lazyness.Stream
import monoids.{Monoid, Stub, WC}
import propertytesting2.Gen.**
import propertytesting2.{Gen, Prop, SGen}
import propertytesting2.Prop.Result
import state.RNG

object JustRunner {
  def main(args: Array[String]): Unit = {

    val r0 = RNG.simple(0)
    def run[A](seed: Int)(g: Gen[A]): A =
      g.sample.run(RNG.simple(seed))._1
//    def run0[A](g: Gen[A]) = run[A](0)(g)
//    def run1[A](g: Gen[A]) = run[A](299)(g)
//    def run2[A](g: Gen[A]) = run[A](98976432)(g)
    def exh[A](g: Gen[A]): List[A] = g.exhaustive.flatMap(
      _.map(Stream(_)).getOrElse(Stream.empty)).toList
    def run0(p: Prop): Result = p.run(100, 5, r0)
    def run1(p: Prop): Result = p.run(5, 20, r0)
    def run2(p: Prop): Result = p.run(100, 100, r0)
    def run3(p: Prop): Result = p.run(11, 500, r0)
    def run_0(p: Prop): Result = p.run(9, 300, r0)
    def monoidLaws[A](m: Monoid[A])(g: SGen[A]): Prop =
      Prop.forAll(g ** g ** g) { case a ** b ** c =>
        val left = m.op(a, m.op(b, c))
        val right = m.op(m.op(a, b), c)
        left == right
      }

    run_0(monoidLaws[WC](Monoid.wcMonoid)(Gen.string.map(Stub)))


    val m = Monoid.wcMonoid
    // Left(((Stub(Cx),Stub( 9)),Stub(L )))
//    val a = Stub("Cx")
//    val b = Stub(" 9")
//    val c = Stub("L ")
    //Left(((Stub(),Stub()),Stub()))
    val a = Stub("")
    val b = Stub("")
    val c = Stub("")
    val left = m.op(a, m.op(b, c))
    val right = m.op(m.op(a, b), c)
    right == left
  }

}