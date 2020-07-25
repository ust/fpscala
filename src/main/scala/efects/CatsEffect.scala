package efects

import cats.effect.IO

object CatsEffect extends App {

  IO(println("DONT DO THAT")).flatMap(_ => IO(print("map"))).unsafeRunSync()

}
