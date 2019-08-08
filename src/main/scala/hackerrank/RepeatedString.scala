package hackerrank

object RepeatedString {

  // Complete the repeatedString function below.
  def repeatedString(s: String, n: Long): Long = {
    val iterations = n / s.length
    val rest = n % s.length
    (for {
      i <- 0 to (s.length - 1)
      if s(i) == 'a'
    } yield if (i < rest) iterations + 1 else iterations).sum
  }

  def main(args: Array[String]): Unit = {
    println(repeatedString("b", 1) == 0)
    println(repeatedString("a", 1) == 1)
    println(repeatedString("ab", 1) == 1)
    println(repeatedString("aba", 2) == 1)
    println(repeatedString("aba", 3) == 2)
    println(repeatedString("aba", 4) == 3)
  }
}
