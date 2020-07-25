package hackerrank

object FrequencyQuery extends App {

  // Complete the freqQuery function below.
  def freqQuery(queries: Array[Array[Int]]): Array[Int] = {
    type Counter = (Map[Int, Int], Map[Int, Int])
    type Acc = (Counter, Seq[Int])
    val zeroAcc = ((Map.empty[Int, Int], Map.empty[Int, Int]), Seq.empty[Int])

    def add(acc: Counter, n: Int): Counter = {
      val (counters, frequencies) = acc
      counters.get(n).map { counter =>
        val newCounters = counters.updatedWith(n)(_ => Some(counter + 1))
        val newFrequencies = frequencies.updatedWith(counter) {
          case Some(1) => None
          case Some(c) => Some(c + 1)
        }.updatedWith(counter + 1) {
          case Some(f) => Some(f + 1)
          case _       => Some(1)
        }
        (newCounters, newFrequencies)
      } getOrElse {
        val newFrequencies = frequencies.updatedWith(1) {
          case Some(f) => Some(f + 1)
          case _       => Some(1)
        }
        (counters + (n -> 1), newFrequencies)
      }
    }

    def remove(acc: Counter, n: Int): Counter = {
      val (counters, frequencies) = acc
      counters.get(n).map {
        case 1       =>
          val newFrequencies = frequencies.updatedWith(1) {
            case Some(1) => None
            case Some(n) => Some(n - 1)
          }
          (counters.removed(n), newFrequencies)
        case counter =>
          val newFrequencies = frequencies.updatedWith(counter) {
            case Some(1) => None
            case Some(f) => Some(f - 1)
          }
          (counters.updatedWith(n)(_ => Some(counter - 1)), newFrequencies)
      } getOrElse {
        (counters, frequencies)
      }
    }

    def query(acc: Counter, n: Int): Boolean = acc._2.contains(n)

    queries.foldLeft(zeroAcc) { case ((counters, result), Array(operation, number)) =>
      operation match {
        case 1 => ( add(counters, number), result )
        case 2 => ( remove(counters, number), result )
        case 3 => ( counters, (if (query(counters, number)) 1 else 0) +: result)
      }
    }._2.reverse.toArray
  }

  def printResult(a: Array[Int], e: Array[Int]): Unit = println(e.mkString + " === " + a.mkString)

  val q1 = Array((1, 5), (1, 6), (3, 2), (1, 10), (1, 10), (1, 6), (2, 5), (3, 2)).map(p => Array(p._1, p._2))
  val r1 = Array(0, 1)
  printResult(freqQuery(q1), r1)

  val q2 = Array((3, 4), (2, 1003), (1, 16), (3, 1)).map(p => Array(p._1, p._2))
  val r2 = Array(0, 1)
  printResult(freqQuery(q2), r2)

  val q3 = Array((1, 3), (2, 3), (3, 2), (1, 4), (1, 5), (1, 5), (1, 4), (3, 2), (2, 4), (3, 2)).map(p => Array(p._1, p._2))
  val r3 = Array(0, 1, 1)
  printResult(freqQuery(q3), r3)
}
