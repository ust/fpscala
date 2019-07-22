package hackerrank

import java.io._

object Solution {

  // Complete the sockMerchant function below.
  def sockMerchant(n: Int, ar: Array[Int]): Int = {
    val init = (new Array[Boolean](100), 0)
    ar.foldLeft(init) { case ((singles, pairs), color) =>
      val a = color - 1
      singles(a) = !singles(a)
      (singles, if (singles(a)) pairs else pairs + 1)
    }._2
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val ar = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = sockMerchant(n, ar)

    printWriter.println(result)

    printWriter.close()
  }
}
