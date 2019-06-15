package wixtest

sealed trait Cell[+A]
case class Tile[+A](value: A) extends Cell[A]
case object Void extends Cell[Nothing]

sealed trait Move
case object Up extends Move
case object Down extends Move
case object Right extends Move
case object Left extends Move

case class Frame[+A: Comparable](cells: Vector[Cell[A]], size: Int) {
  def move(move: Move): Frame[A] = ???
  def moves: Stream[Frame[A]] = ???
  def flatMap[B](f: A => Frame[B]): Frame[B] = ???
  def map[B](f: A => B): Frame[B] = ???
}

object Frame {
  def cons[A: Comparable](values: Vector[A], size: Int, empty: Int): Frame[A] = {
    // todo require( sqrt ???)
    val cells = values.map(v => Tile(v)).splitAt(empty) match {
      case (l, r) => (l :+ Void) ++ r
    }
    Frame(cells, size)
  }

  def isSolved[A: Comparable](frame: Frame[A]): Boolean = ???
}

object Solver {
  def rand[A](): Frame[A] = ???
  def solve[A](frame: Frame[A]): Stream[Frame[A]] = ???
}

