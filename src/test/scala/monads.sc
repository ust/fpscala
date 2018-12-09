import propertytesting.Gen._
import propertytesting.Prop.Result
import propertytesting.{Gen, Prop}
import state.RNG

val r0 = RNG.simple(0)
def run0(p: Prop): Result = p.run(10, 500, r0)
val genOptInt: Gen[Option[Int]] = (Gen.int ** Gen.boolean)
  .map { case i ** b => if (b) Some(i) else None }
val genFnIntInt: Gen[Int => Option[Int]] =
  Gen.int.map[Int => Option[Int]] { i =>
    if (i % 2 == 1) Some(_) else _ => None
  }

run0(Prop.forAll((genOptInt ** genFnIntInt ** genFnIntInt).unsized) {
  case ((x, f), g) =>
    x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
})

