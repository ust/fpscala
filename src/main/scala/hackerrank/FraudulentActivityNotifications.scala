package hackerrank

object FraudulentActivityNotifications extends App {

  // Complete the activityNotifications function below.
  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    type Acc = (Seq[Int], Int, Int)
    val acc: Acc = (Seq.empty[Int], 0, 0)

    def search(seq: Seq[Int], a: Int): Int = {
      @annotation.tailrec
      def go(start: Int, end: Int): Int =  {
        val d = end - start
        val i = start + d / 2
        (a compare seq(i), d) match {
          case ( 0, _) => i
          case (-1, 1) => start
          case ( 1, 1) => end
          case (-1, _) => go(start, i)
          case ( 1, _) => go(i, end)
        }
      }

      if (seq.isEmpty) 0 else go(0, seq.size)
    }

    def insert(seq: Seq[Int], a: Int): Seq[Int] = {
      val i1 = search(seq, a)
      //println(i1)
      seq.patch(i1, Seq(a), 0)
    }

    def drop(seq: Seq[Int], a: Int): Seq[Int] = seq.span(_ != a) match { case (l, r) => l ++ r.tail}

    def median2x(l: Seq[Int]): Int = if (l.size < d) -1 else (l.size % 2, l.size / 2) match {
      case (0, n) => l.view.slice(n - 1, n + 1).sum
      case (1, n) => l(n) * 2
    }

    expenditure.foldLeft(acc) { case ((sequence, i, counter), a) =>
      val m = median2x(sequence)
      val first = i - d
      val newSeq = if (sequence.size >= d) insert(drop(sequence, expenditure(first)), a) else insert(sequence, a)

      println(s"$i, $newSeq, median: $m, first: $first, a: $a")

      (newSeq, i + 1, if (i >= d && m <= a) counter + 1 else counter)
    }._3
  }

  def printResult(a: Int, e: Int): Unit = println (s"actual: $a   expected: $e")

  val e1 = Array(2, 3, 4, 2, 3, 6, 8, 4, 5)
  //printResult(activityNotifications(e1, 5), 2)

  val e2 = Array(1, 2, 3, 4, 4)
  printResult(activityNotifications(e2, 4), 0)

}
