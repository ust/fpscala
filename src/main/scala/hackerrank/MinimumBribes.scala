package hackerrank

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object MinimumBribes {

  def minimumBribes2(q: Array[Int]): String = {
    type Number = Int; type Bribes = Int
    type Position = (Number, Bribes)
    type Queue = List[Position]
    type State = List[Queue]
    // closer to aim, less moves
    implicit lazy val ordering: Ordering[State] = Ordering[(Int, Int)].on[State]((s: State) => (howFar(s), bribes(s)))

    def predicate(state: State): Boolean = hasMoreBribes(state)
    def hasMoreBribes(state: State): Boolean = state.head.exists { case (_, b) => b > 0 }
    // absolute error
    def howFar(state: State): Int = state.head.foldLeft((1, 0))((acc, q) => (acc._1 + 1, acc._2 + (acc._1 - q._1).abs))._2
    def isFinished(state: State): Boolean = state.head.zipWithIndex.forall { case (p, i) => p._1 == i + 1}
    def state(array: Array[Int]): State = List(array.map(n => (n, 2)).toList)
    def prevS(state: State): List[State] = prevQ(state.head).map(_ :: state)
    def prevQ(queue: Queue): List[Queue] = (0 to queue.size).toList flatMap (swap(queue)(_))
    def swap(queue: Queue)(index: Int): Option[Queue] = queue.splitAt(index) match {
      case (right, (number, bribes) :: a :: tail) if bribes > 0 => Some(right ++ (a :: (number, bribes - 1) :: tail))
      case _                                               => None
    }
    def bribes(state: State): Int = state.size - 1
    @tailrec
    def go(states: SortedSet[State]): Option[State] = {
      //println(states.headOption.map(s => s"err:${howFar(s)} $s").getOrElse("empty"))
      states.headOption match {
        case res@Some(s) => if (isFinished(s)) res else go(states.tail ++ prevS(s))
        case _         => None
      }
    }

    // put initial to the queue
    // get from queue, check finish. Finish == ordered
    // generate prev states and put to queue. Prev != ( (a[i] > i+2)  && (everyone spent 2 bribes) )
    go(SortedSet(state(q))).map(bribes(_).toString).getOrElse("Too chaotic")
  }

  // Complete the minimumBribes function below.
  def minimumBribes999(q: Array[Int]): String  = {
    @tailrec
    def go(left: Set[Int], right: Set[Int], bribes: Int)(i: Int): Int = {
      if (i > q.length) bribes else {
        val a = q(i - 1)
        println(s"[i:$i, a:$a, ${left.mkString("(", ",", ")")}<-${right.mkString("(", ",", ")")}, bribes:$bribes] ")
        // remove found a[i] from accumulated set // + (a == i).compareTo(false)
        if (right contains a) go(left - i, right - a, bribes + right.size - 1)(i + 1)
        else if (a < i || a > i + 2) -1 // Too chaotic
        // else add all [ i .. a[i] ) to accumulated, bribes += right.size
        else {
          val bribed = right ++ (i until a).filterNot(left contains)
          go(left + a - i, bribed, bribes + bribed.count(_ <= i + right.size))(i + 1) // size -> count(_ <= i)
        } // 2 3 5 1 4
      }
    }
    Some(go(Set.empty, Set.empty, 0)(1)) filter(_ >= 0) map(_.toString) getOrElse  "Too chaotic"
  }

  def minimumBribes88(q: Array[Int]): String = {
    // define persons with bribed and bribes counters
    type Bribes = Int
    type Bribed = Int
    type Person = (Bribes, Bribed)
    @tailrec
    def go(persons: Vector[Person])(i: Int): Vector[Person] = {
      val a = q(i - 1)
      val d = a - i
      // validate chaos
      if (d > 2) Vector.empty else {
        val (prev, next) = persons.splitAt(i - 1)
        // update prev elements
        // add/upd next. bribed += 1
        // add/upd person. bribes = d/0, bribed = 0/_
        go(prev ++ Seq((d, 0)) ++ next)(i + 1)
      }
    }
    // calc sum bribes or bribed
    def bribes(queue: Seq[Person]): Int = if (queue.isEmpty) -1 else ???
    Some(bribes(go(Vector.empty)(1))) filter (_ < 0) map (_.toString) getOrElse  "Too chaotic"
  }

  def minimumBribes(q: Array[Int]): String = {
    @tailrec
    def go(bribes: Int, bribed: List[(Int, Int)])(i: Int): Int = {
      if (i > q.length) bribes else {
        val a = q(i - 1)
        val d = a - i
        if (d > 2) -1
        // if d+   -> bribed ++ i..a;  bribed.each(_ + 1); d = a-b;   bribes += d
        else if (d > 0)
          go(bribes + d, bribed.map{ case (b, n) => (b, n + 1)} ++ (i until a).map(_ -> 1))(i + 1)
        // else d- -> bribed - a;      bribed.each(_ + 1); d = a+b-i; bribes += d
        else {
          val b = bribed find (_._1 == a) map (a + _._2 - i) getOrElse 0
          go(bribes + b, bribed.filter(_._1 == a))(i + 1)
        }
      }
    }
    Some(go(0, List.empty)(1)) filter (_ >= 0) map (_.toString) getOrElse  "Too chaotic"
  }

  def main(args: Array[String]): Unit = {
    println(minimumBribes(Array(1, 2)) + " 0") // 0 0
    println(minimumBribes(Array(2, 1)) + " 1") // +1 -1
    println(minimumBribes(Array(3, 1, 2)) + " 2\n") // +2 -1 -1
    println(minimumBribes(Array(1, 3, 2)) + " 1") // 0 +1 -1
    println(minimumBribes(Array(1, 3, 2, 4)) + " 1") // 0 +1 -1 0
    println(minimumBribes(Array(2, 1, 3, 4)) + " 1") // +1 -1 0 0
    println(minimumBribes(Array(1, 2, 3, 4, 5)) + " 0") // 0 0 0 0 0
    println(minimumBribes(Array(3, 2, 5, 4, 1)) + " 6")
    println(minimumBribes(Array(2, 1, 3, 4, 5)) + " 1")
    println(minimumBribes(Array(3, 1, 2, 4, 5)) + " 2\n")
    println(minimumBribes(Array(2, 1, 5, 3, 4)) + " 3\n")
    println(minimumBribes(Array(2, 3, 4, 5, 1)) + " 4") // +1 +1 +1 +1 -4
    println(minimumBribes(Array(2, 3, 5, 4, 1)) + " 5") // +1 +1 +2 0 -4
    println(minimumBribes(Array(2, 3, 5, 4, 1, 6, 7, 8)) + " 5") // +1 +1 +2 0 -4 0 0 0
    println(minimumBribes(Array(2, 3, 5, 6, 4, 1, 7, 8)) + " 7\n") // 1/0 1/0 2/0 2/0 (4i+2b-5p)/2 (1i+5b-6p)/5 0/0 0/0 ()
    println(minimumBribes(Array(2, 3, 5, 6, 4, 7, 1, 8)) + " 8\n") // 1/0 1/0 2/0 6/0 (4i+2b-5p)/2 1/0 (1i+6b-7p)/6 0/0 ()
    println(minimumBribes(Array(2, 5, 1, 3, 4)) + " Too chaotic")
  }
}
