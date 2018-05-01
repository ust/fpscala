import java.util.concurrent.{ExecutorService, Executors}

import parallelism._
import propertytesting.Gen._
import propertytesting.Prop._
import propertytesting._
import state._

def print[A](s: State[RNG, A], l: RNG) = s.run(l)._1
def print[A](g: Gen[A], l: RNG) = g.sample.run(l)._1
def print[A](g: Gen[A]) =
  g.exhaustive.filter(_.isDefined).map(_.get).toList
val rng = RNG.simple(0)
val rng1 = RNG.simple(1245341)
val rng2 = RNG.simple(12431)

val smallInt = choose(-10, 10)
val assertListMax: List[Int] => Boolean = l => {
  val max = l.max
  !l.exists(_ > max)
}
val assertListSort: List[Int] => Boolean = l => {
  val ls = l.sorted
  ls.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
}
val ES: ExecutorService = Executors.newCachedThreadPool

run(forAll(listOf(smallInt))(assertListMax))
run(forAll(listOf1(smallInt))(assertListMax))
run(forAll(listOf(smallInt))(assertListSort))
run(forAll(unit(Par.unit(1)))(i =>
  Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get))
run(check {
  val p = Par.map(Par.unit(1))(_ + 1)
  val p2 = Par.unit(2)
  p(ES).get == p2(ES).get
})

print(uniform)
print(uniform, rng)
print(uniform, rng1)
print(uniform, rng2)
print(choose(-12.2, 5.89))
print(choose(-12.2, 5.89), rng)
print(choose(-12.2, 5.89), rng1)
print(choose(-12.2, 5.89), rng2)
print(choose(-3, 5))
print(choose(-3, 5), rng1)
print(boolean)
print(boolean, rng)
print(boolean, rng1)
print(choose(1, 1).listOfN(2))
print(choose(1, 2).listOfN(2))
print(choose(1, 4).listOfN(1))
print(choose(1, 3).listOfN(2))
print(choose(1, 3).listOfN(3))
print(choose(1, 4).listOfN(2))
print(choose(1, 3).listOfN(0))
print(choose(1, 3).listOfN(2), rng1)
print(sameParity(1, 4))
print(randomListOf(choose(1, 6)), rng1)
print(randomListOf(character), rng1)
print(integer, rng2)
print(character, rng2)
print(short, rng2)
print(string, rng2)
print(union(choose(0, 3), choose(3, 5)), rng)

print(weighted((choose(0, 5), 0.5),
  (choose(5, 10), 0.5)), rng2)
print(weighted((choose(0, 5), 0.3),
  (choose(5, 10), 0.7)), rng2)
print(union(choose(0, 3), choose(3, 5)), rng1)
print(union(choose(0, 3), choose(3, 6)))



