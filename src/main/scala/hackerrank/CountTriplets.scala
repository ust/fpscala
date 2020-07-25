package hackerrank

import scala.collection.immutable.SortedSet

object CountTriplets {
  // BRUTE
  // each i -> j -> k
  // where i < j < k
  // where a[i]^2 == a[j] and a[j]^2 == a[k]
  def countTriplets2(arr: Array[Long], r: Long): Long = {
    // map elements on duplicates count
    val indices = arr.zipWithIndex.groupMap(_._1)(_._2).map { case (k, a) => (k, a.to(SortedSet)) }
    // create progressive triplets from keys
    val triplets = for {
      n <- indices.keys
      i <- indices.getOrElse(n, Seq.empty)
      j <- indices.getOrElse(n * r, SortedSet.empty[Int]).iteratorFrom(i + 1)
      k <- indices.getOrElse(n * r * r, SortedSet.empty[Int]).iteratorFrom(j + 1)
    } yield (i, j, k)
    triplets.size
  }

  // Complete the countTriplets function below.
  def countTriplets(arr: Array[Long], r: Long): Long = {
    // map elements on duplicates count
    val indices = arr.zipWithIndex.groupMap(_._1)(_._2).map { case (k, a) => (k, a.to(SortedSet)) }
    val elements = indices.keys.to(SortedSet).toSeq
    val triplets = for {
      e <- elements
      is = indices.getOrElse(e, SortedSet.empty[Int])
      js = indices.getOrElse(e * r, SortedSet.empty[Int])
      ks = indices.getOrElse(e * r * r, SortedSet.empty[Int])
      if is.nonEmpty && js.nonEmpty && ks.nonEmpty
      i <- is
      j <- js.iteratorFrom(i + 1)
      k <- js.iteratorFrom(j + 1)
    } yield (i, j, k)
    triplets.size
  }

  def main(args: Array[String]): Unit = {
    println(countTriplets2(Array(1, 1, 1), 1) + " 1")
    println(countTriplets2(Array(1, 1, 1, 1, 1, 1), 1) + " 20")
    println(countTriplets2(Array(1, 1, 1, 9, 9, 9), 1) + " 2")
    println(countTriplets2(Array(1, 1, 1, 2, 2, 2, 9, 9, 9), 1) + " 3")
    println(countTriplets2(Array(1, 3, 9, 1, 3, 9), 3) + " 4")
    println ( "-------------------")
    println(countTriplets(Array(1, 3, 9, 9, 27, 81), 3) + " 6")
    println(countTriplets(Array(1, 9, 3, 9, 27, 81), 3) + " 4")
    println(countTriplets(Array(81, 27, 9, 3, 9, 1), 3) + " 0")

    //println(countTriplets(Array(1, 3, 9, 1, 3, 9), 3) + " 4")
    println ( "-------------------")
    println(countTriplets(Array(1, 1, 3, 3, 3, 9, 9, 9), 3) + " 18")
    println(countTriplets(Array(1, 3, 1, 3, 3, 9, 9, 9), 3) + " 15")
    println(countTriplets(Array(1, 3, 1, 3, 9, 3, 9, 9), 3) + " 13")
    println(countTriplets(Array(3, 1, 1, 3, 3, 9, 9, 9), 3) + " 12")
    println(countTriplets(Array(3, 1, 1, 3, 9, 3, 9, 9), 3) + " 10")
  }
}
