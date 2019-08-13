package hackerrank

object CountingValleys {

  // Complete the countingValleys function below.
  def countingValleys(n: Int, s: String): Int = {
    s.foldLeft[(Int, Int)]((0, 0))((acc, c) => (acc._1, acc._2, c) match {
      case (level, vallyes, 'U') => (level + 1, if (level == -1) vallyes + 1 else vallyes)
      case (level, vallyes,  _ ) => (level - 1, vallyes)
    })._2
  }

  def main(args: Array[String]): Unit = {
    println(countingValleys(0, "UD") == 0)
    println(countingValleys(0, "DU") == 1)
    println(countingValleys(0, "DDUU") == 1)
    println(countingValleys(0, "UUDDDDUU") == 1)
    println(countingValleys(0, "UUDDUUUDDDUUUUUDD") == 0)
  }

}
