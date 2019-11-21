package hackerrank

import scala.collection.immutable.SortedSet

object CountTriplets {
  // BRUTE
  // each i -> j -> k
  // where i < j < k
  // where a[i]^2 == a[j] and a[j]^2 == a[k]
  def countTriplets2(arr: Array[Long], r: Long): Long = {
    // map elements on duplicates count
    val indices = arr.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).to[SortedSet])
    // create progressive triplets from keys
    val triplets = for {
      n <- indices.keys
      i <- indices.getOrElse(n, Seq.empty)
      j <- indices.getOrElse(n * r, SortedSet.empty[Int]).keysIteratorFrom(i + 1)
      k <- indices.getOrElse(n * r * r, SortedSet.empty[Int]).keysIteratorFrom(j + 1)
    } yield (i, j, k)

    triplets.size
  }

  // Complete the countTriplets function below.
  def countTriplets(arr: Array[Long], r: Long): Long = {
    // map elements on duplicates count
    val indices = arr.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).to[SortedSet])
    // iterate through all keys and extract their progressive versions and
    // count (k->v) k = size of min set (i in j) v = i less then some j. Count j->k version
    // iterate ijs and jks and calc
    val triplets = for {
      n <- indices.keys
      is = indices.getOrElse(n, SortedSet.empty[Int])
      js = indices.getOrElse(n * r, SortedSet.empty[Int])
      ks = indices.getOrElse(n * r * r, SortedSet.empty[Int])
      ij = is.map(js.keysIteratorFrom(_).size).groupBy(identity).mapValues(_.size)
      jk = js.map(ks.keysIteratorFrom(_).size).groupBy(identity).mapValues(_.size)
      (a, b) <- ij
      (c, d) <- jk
    } yield b * a * ???

    triplets.sum
  }

  //                               [(gtNum:lsSet)-> counter]
  // (1,2) (3,4,5) (6,7,8) = 18 => (3->2) (3->3)            = 2->3->3
  // (1,3) (2,4,5) (6,7,8) = 15 => (3->1;2->1) (3->3)       = 1->3->3; 1->2->3
  // (1,3) (2,4,6) (5,7,8) = 13 => (3->1;2->1) (3->2,2->1)  = 1->2->3; 1->1->2; 1->2->2; 1->(2-1)->2
  // (2,3) (1,4,5) (6,7,8) = 12 => (2->2) (3->2)            = 2->2->3
  // (2,3) (1,4,6) (5,7,8) = 10 => (2->2) (3->1;2->1)       = 2->1->3; 2->1->2

  def main(args: Array[String]): Unit = {
    println(countTriplets(Array(1, 1, 1, 1, 1, 1), 1) + " 20")
    println(countTriplets(Array(1, 1, 1, 9, 9, 9), 1) + " 2")
    println(countTriplets(Array(1, 3, 9, 9, 27, 81), 3) + " 6")
    println(countTriplets(Array(1, 9, 3, 9, 27, 81), 3) + " 4")
    println(countTriplets(Array(1, 1, 3, 3, 3, 9, 9, 9), 3) + " 18")
    println(countTriplets(Array(1, 3, 1, 3, 3, 9, 9, 9), 3) + " 15")
    println(countTriplets(Array(1, 3, 1, 3, 9, 3, 9, 9), 3) + " 13")
    println(countTriplets(Array(3, 1, 1, 3, 3, 9, 9, 9), 3) + " 12")
    println(countTriplets(Array(3, 1, 1, 3, 9, 3, 9, 9), 3) + " 10")
  }
}
