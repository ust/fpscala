object Misc {

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  // exercise 5
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  //    (a: A, b: B) => f(a)(b)
    f(_)(_)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  //    (a: A) => (b: B) => f(a, b)
    (a) => f(a, _)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean =
      if (i == as.length) true
      else if (gt(as(i - 1), as(i))) false
      else go(i + 1)

    if (as.length < 2) true else go(1)
  }

  def binarySearch[A](ds: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2)
        if (d == key) mid2
        else {
          val greater = gt(d, key)
          if (greater) go(low, mid2, mid2 - 1)
          else go(mid2 + 1, mid2, high)
        }
      }
    }

    go(0, 0, ds.length - 1)
  }

  def fib(n: Int): Int = {
    def f(n: Int) = n match {
      case 1 => 0
      case 2 => 1
      case x => go(x - 2, 0, 1)
    }

    @annotation.tailrec
    def go(n: Int, a: Int, b: Int): Int =
      if (n == 0) b else go(n - 1, b, a + b)

    if (n <= 0) -999 else f(n)
  }
}
