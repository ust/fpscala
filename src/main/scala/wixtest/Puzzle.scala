package wixtest

// - presentation (play mode, shortest solved)
// - tests, property-tests
// - expressive, library, laws like cannot move
// - extends ? (4 - n, dimensions?)

sealed trait Cell[+A]
case class Tile[+A](value: A) extends Cell[A]
case object Void extends Cell[Nothing]

sealed trait Move
case object Up extends Move
case object Down extends Move
case object Right extends Move
case object Left extends Move

case class Frame[+A: Comparable[A]](size: Int) {
  def move(move: Move): Frame[A] = ???
  def moves: Stream[Frame[A]] = ???
  def flatMap[B](f: A => Frame[B]): Frame[B] = ???
  def map[B](f: A => B): Frame[B] = ???
}

object Frame {
//  def Frame[A]: Frame[A] = ???

  def shit: Frame[Boolean] = {
    for {
      t <- Frame[Int](8)
    } yield t > 0
  }
}

object Solver {
  def rand[A](): Frame[A] = ???
  def solve[A](frame: Frame[A]): Stream[Frame[A]] = ???
}

