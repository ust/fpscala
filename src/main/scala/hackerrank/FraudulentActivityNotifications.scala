package hackerrank

object FraudulentActivityNotifications extends App {

  // Complete the activityNotifications function below.
  def activityNotifications(expenditure: Array[Int], d: Int): Int = {
    type Acc = (Vector[Int], Int, Int)
    val acc: Acc = (Vector.empty[Int], 0, 0)

    def biSearch(vector: Vector[Int], a: Int): Int = {
      @annotation.tailrec
      def go(start: Int, end: Int): Int =  {
        val d = end - start
        val i = start + d / 2
        (a compare vector(i), d) match {
          case ( 0, _) => i
          case (-1, 1) => start
          case ( 1, 1) => end
          case (-1, _) => go(start, i)
          case ( 1, _) => go(i, end)
        }
      }

      if (vector.isEmpty) 0 else go(0, vector.size)
    }

    def insert(vector: Vector[Int], a: Int): Vector[Int] = vector.patch(biSearch(vector, a), Seq(a), 0)

    def drop(vector: Vector[Int], a: Int): Vector[Int] = vector.patch(biSearch(vector, a), Nil, 1)

    def median2x(l: Vector[Int]): Int = if (l.size < d) -1 else (l.size % 2, l.size / 2) match {
      case (0, n) => l(n - 1) + l(n)
      case (1, n) => l(n) * 2
    }

    expenditure.foldLeft(acc) { case ((vector, i, counter), a) =>
      val m = median2x(vector)
      val first = i - d
      val newVector = if (vector.size >= d) insert(drop(vector, expenditure(first)), a) else insert(vector, a)

      //println(s"$i, $newVector, median: $m, first: $first, a: $a")

      (newVector, i + 1, if (i >= d && m <= a) counter + 1 else counter)
    }._3
  }

  def printResult(a: Int, e: Int): Unit = println (s"actual: $a   expected: $e")

  val e1 = Array(2, 3, 4, 2, 3, 6, 8, 4, 5)
  //printResult(activityNotifications(e1, 5), 2)

  val e2 = Array(1, 2, 3, 4, 4)
  printResult(activityNotifications(e2, 4), 0)

}
