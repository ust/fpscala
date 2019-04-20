import wixtest.Frame
// test random + arbitrary

val int15 = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
val frame15Solved = Frame.cons(int15, 4, 16)
require(Frame.isSolved(frame15Solved))

// test solver (A*)
// -- isSolved (on construction)
// -- heuristic sort + check existed (replace by lower path)
// ---- path to the starting point

// client (cli)

// test prove solvable

// expressiveness ? (size, monad, algorithm)

// pack: project sbt, git
