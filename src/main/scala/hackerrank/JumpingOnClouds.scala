package hackerrank

object JumpingOnClouds {

  // Complete the jumpingOnClouds function below.
  def jumpingOnClouds(c: Array[Int]): Int = {
    def jumps(j: Int, p: Int): Int = c.length - p - 1 match {
      case 0 => j
      case 1 => j + 1
      case _ => (c(p + 1), c(p + 2)) match {
        case (0, 1) => jumps(j + 1, p + 1)
        case (1, 0) => jumps(j + 1, p + 2)
        case _      => jumps(j + 1, p + 1) min jumps(j + 1, p + 2)
      }
    }
    jumps(0, 0)
  }

  def main(args: Array[String]): Unit = {
    println(jumpingOnClouds(Array(0, 0)) == 1)
    println(jumpingOnClouds(Array(0, 0, 0)) == 1)
    println(jumpingOnClouds(Array(0, 1, 0)) == 1)
    println(jumpingOnClouds(Array(0, 0, 0, 0)) == 2)
    println(jumpingOnClouds(Array(0, 0, 1, 0)) == 2)
    println(jumpingOnClouds(Array(0, 0, 0, 0, 0)) == 2)
    println(jumpingOnClouds(Array(0, 0, 1, 0, 0)) == 3)
    println(jumpingOnClouds(Array(0, 0, 0, 0, 0, 0)) == 3)
  }
}
