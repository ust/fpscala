package wixtest

// - presentation (play mode, shortest solved)
// - tests, property-tests
// - expressive, library, laws like cannot move
// - extends ? (4 - n, dimensions?)

sealed trait Cell[+A]
case class Tile[+A](value: A) extends Cell[A]
case object Void extends Cell[Nothing]

sealed trait Move
case object Plus extends Move
case object Minus extends Move
case object Stay extends Move

case class Axis[A]() {
  def move() = ???
}
