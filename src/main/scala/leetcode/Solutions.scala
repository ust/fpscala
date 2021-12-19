package leetcode

object Solutions {
  /**
   * println(Solution.twoSum(Array(2, 7, 10, 4), 9).toSeq)
   * println(Solution.twoSum(Array(3, 2, 4), 6).toSeq)
   * println(Solution.twoSum(Array(3, 3), 6).toSeq)
   * println(Solution.twoSum(Array(1, 3, 3, 3, 2), 6).toSeq) */
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val indexes = nums.zipWithIndex.groupBy(_._1)

    def go(a: Int, i: Int): Option[Int] =
      indexes.get(target - a).flatMap(_.dropWhile(_._2 <= i).headOption.map(_._2))

    nums.zipWithIndex.view.flatMap { case (a, i) => go(a, i).map(Array(i, _)) }.head
  }
}

object Solution {
  def searchInsert(nums: Array[Int], target: Int): Int = {
    @scala.annotation.tailrec
    def go(l: Int, h: Int): Int = {
      val i = (l + h) / 2
      val d = h - l
      if (d == 1) if (nums(i) >= target) i else i + 1
      else if (nums(i) == target) i
      else if (nums(i) > target) go(l, i) else go(i, h)
    }
    if (nums.isEmpty) 0 else go(0, nums.length)
  }
}

object Runner extends App {
  println(Solution.searchInsert(Array(), target = 5) + " should be 0")
  println(Solution.searchInsert(Array(1), target = 5) + " should be 1")
  println(Solution.searchInsert(Array(1,3,5,6), target = 5) + " should be 2")
  println(Solution.searchInsert(Array(1,3,5,6), target = 2) + " should be 1")
  println(Solution.searchInsert(Array(1,3,5,6), target = 7) + " should be 4")
}