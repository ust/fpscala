import scala.annotation.tailrec

object MergeRanges {

  def main(args: Array[String]): Unit = {
    val aa = -10 to -4
    val bb = -3 to 1
    val a = 1 to 4
    val b = 3 to 5
    val c = 6 to 8
    val d = 8 to 10
    val dd = -2 to 10

    println(merge(aa, a))
    println(merge(aa, bb))
    println(merge(a, b))
    println(merge(c, b))
    println(merge(c, d))
    println(merge(a, d))

    println(brute(List(a, b, c, d)))
    println(brute(List(a, b, c, d, dd)))
  }

  def brute(rs: List[Range]): List[Range] = {
    @tailrec
    def go(left: List[Range], right: List[Range]): (Option[Range], List[Range]) = {
      left.tail.foldLeft[(Option[Range], List[Range])]((None, Nil))((acc, b) => {
        merge(left.head, b) match {
          case a@Some(_) => (a, acc._2)
          case _         => (acc._1, b :: acc._2)
        }
      }) match {
        case (None, Nil)  => (None, left.head :: right)
        case (None, _)    => go(left.tail, left.head :: right)
        case (Some(a), l) => go(a :: l, right)
      }
    }

    if (rs.nonEmpty) go(rs, Nil)._2 else rs
  }

  def merge(a: Range, b: Range): Option[Range] = {
    val (left, right) =  if (a.start <= b.start) (a, b) else (b, a)
    (left.start, left.end, right.start, right.end) match {
      case (_, l, _, r)     if l >= r   => Some(left)
      case (ls, le, rs, re) if le >= rs => Some(ls to re)
      case _                            => None
    }
  }

}



