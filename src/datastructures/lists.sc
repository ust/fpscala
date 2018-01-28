import datastructures.{Cons, List, Nil, Stream}

def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
  def go(a: List[A], b: List[A]): Boolean =
    a match {
      case Cons(ah, at) => b match {
        case Cons(bh, bt) =>
          go(at, if (ah == bh) bt else sub)
        case Nil => true
      }
      case Nil => b == Nil
    }

  go(l, sub)
}

def hasSubsequence2[A](l: List[A], sub: List[A]): Boolean = {
  Stream.unfold((l, sub))(s => s._2 match {
    case Cons(sh, st) => s._1 match {
      case Cons(ah, at) =>
        if (ah == sh) scala.Some(false, (at, st))
        else scala.Some(false, (at, sub))
      case Nil => scala.None
    }
    case Nil => scala.Some(true, (Nil, Nil))
  }).exists(_ == true)
}


hasSubsequence2(List(1, 2, 4, 1, 2, 3, 4), List(1, 2, 3))
hasSubsequence(List(1, 2, 4, 1, 2, 3, 4), List(1, 2, 3))