package hackerrank

object CountTriplets {
  // BRUTE
  // each i -> j -> k
  // where i < j < k
  // where a[i]^2 == a[j] and a[j]^2 == a[k]

  // Complete the countTriplets function below.
  def countTriplets(arr: Array[Long], r: Long): Long = {
    // map elements on duplicates count
    val grouped = arr.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).sorted)
    // sort keys
    val keys = grouped.keys.toSeq.sorted
    // create progressive triplets from keys
    val ranges = if (r == 1) (keys.indices, keys.indices, keys.indices)
    else (0 until keys.size - 2, 1 until keys.size - 1, 2 until keys.size)

    val triplets =
      for {
        a <- ranges._1
        b <- ranges._2
        if (r == 1 || a < b) && r * keys(a) == keys(b)
        c <- ranges._3
        if (r == 1 || b < c) && r * keys(b) == keys(c)
        i <- grouped(keys(a))
        j <- grouped(keys(b))
        if i < j
        k <- grouped(keys(c))
        if j < k
      } yield (i, j, k)
    triplets.size
  }

  def main(args: Array[String]): Unit = {
    println(countTriplets(Array(1, 1, 1, 1, 1, 1), 1) + " 20")
    println(countTriplets(Array(1, 1, 1, 9, 9, 9), 1) + " 2")
    println(countTriplets(Array(1, 3, 9, 9, 27, 81), 3) + " 6")
    println(countTriplets(Array(1, 9, 3, 9, 27, 81), 3) + " 4")
  }
}
