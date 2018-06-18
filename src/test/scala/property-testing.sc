import java.util.concurrent.{ExecutorService, Executors}

import lazyness._
import parallelism._
import propertytesting.Gen._
import propertytesting.Prop._
import propertytesting._
import state._

val rng = RNG.simple(0)
val rng1 = RNG.simple(1245341)
val rng2 = RNG.simple(12431)
val smallInt = choose(-10, 10)
val ES: ExecutorService =
  Executors.newCachedThreadPool

def sample[A](g: Gen[A], l: RNG = rng) =
  g.sample.run(l)._1
def exhaustive[A](g: Gen[A]) =
  g.exhaustive.filter(_.isDefined).map(_.get).toList

run(forAll(listOf(smallInt))(l => {
  val max = l.max
  !l.exists(_ > max)
}))
run(forAll(listOf1(smallInt))(l => {
  val max = l.max
  !l.exists(_ > max)
}))
run(forAll(listOf(smallInt))(l => {
  val ls = l.sorted
  ls.isEmpty || !ls.zip(ls.tail).exists {
    case (a, b) => a > b
  }
}
))
run(forAll(Gen.unit(Par.unit(1)))(i =>
  Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get))
run(check {
  val p = Par.map(Par.unit(1))(_ + 1)
  val p2 = Par.unit(2)
  p(ES).get == p2(ES).get
})
run(check {
  val p = Par.map(Par.unit(1))(_ + 1)
  val p2 = Par.unit(2)
  Par.equal(p, p2)(ES).get()
})
run(checkPar {
  Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))
})
run(checkPar {
  val f: Int => Int = _ * 2
  Par.equal(Par.map(Par.unit(2))(f), Par.unit(f(2)))
})
run(checkPar {
  val f: Int => Int = x => x
  Par.equal(Par.map(Par.unit(2))(f), Par.unit(2))
})
run(forAllPar(smallInt.map(Par.unit(_))) {
  i => Par.equal(i, Par.map(i)(x => x))
})
run(forAllPar(int) {
  i => Par.equal(Par.unit(i), Par.fork(Par.unit(i)))
})
run(forAll {
  randListOf(int) ** genFn(smallInt)(_ < 0)
} {
  case l ** f => l.takeWhile(f).forall(f)
})
run("take") {
  forAll(randListOf(int) ** genFn(smallInt)(_ < 0)) {
    case l ** f => l.takeWhile(f) ++ l.dropWhile(f) == l
  }
}
run("take")(forAll(randListOf(int) ** int) {
  case l ** i =>
    val n = l.size / 2
    l.take(n) ++ l.drop(n) == l
})
run("sorted") {
  forAll(randListOf(int) ** genFn(smallInt)(_ < 0)) {
    case l ** f => l.filter(f).sorted.forall(f)
  }
}
run("unfold") {
  forAll {
    int ** genFn(int)(_ % 3 != 0) ** genFn(int)(_ ^ 2)
  } { case z ** ofn ** sfn =>
    val fn: Int => Option[(Int, Int)] = s =>
      if (ofn(s)) Some((s, sfn(s))) else None

    Stream.unfold(z)(fn).toList.forall(ofn)
  }
}
// TODO genTree
import datastructures.Tree

def tree[A](g: SGen[A])(f: Gen[A => Tree[A]]): SGen[Tree[A]] =
  Gen.flatMap(g)(???)


"^^^^^^^^ you are here ^^^^^^^"
// TODO SGen
// TODO sequence
sample(genIntFn(int), rng1)("hui")
sample(genIntFn(int), rng2)("hui")
sample(genIntFn(int), rng1)("zhopa")
sample(genIntFn(int), rng2)("zhopa")
sample(genFn(int)(_.toString), rng1)("zhopa")
exhaustive(uniform)
sample(uniform, rng)
sample(uniform, rng1)
sample(uniform, rng2)
exhaustive(choose(-12.2, 5.89))
sample(choose(-12.2, 5.89), rng)
sample(choose(-12.2, 5.89), rng1)
sample(choose(-12.2, 5.89), rng2)
exhaustive(choose(-3, 5))
sample(choose(-3, 5), rng1)
exhaustive(boolean)
sample(boolean, rng)
sample(boolean, rng1)
exhaustive(choose(1, 1).listOfN(2))
exhaustive(choose(1, 2).listOfN(2))
exhaustive(choose(1, 4).listOfN(1))
exhaustive(choose(1, 3).listOfN(2))
exhaustive(choose(1, 3).listOfN(3))
exhaustive(choose(1, 4).listOfN(2))
exhaustive(choose(1, 3).listOfN(0))
sample(choose(1, 3).listOfN(2), rng1)
exhaustive(sameParity(1, 4))
sample(randListOf(choose(1, 6)), rng1)
sample(randListOf(char), rng1)
sample(int, rng2)
sample(char, rng2)
sample(short, rng2)
sample(string, rng2)
sample(union(choose(0, 3), choose(3, 5)), rng)

sample(weighted((choose(0, 5), 0.5),
  (choose(5, 10), 0.5)), rng2)
sample(weighted((choose(0, 5), 0.3),
  (choose(5, 10), 0.7)), rng2)
sample(union(choose(0, 3), choose(3, 5)), rng1)
exhaustive(union(choose(0, 3), choose(3, 6)))
