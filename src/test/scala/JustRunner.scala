import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds
import scala.language.postfixOps

object JustRunner {
  case class Thing(name: String)

  implicit val ec = ExecutionContext.global

  def job(args: Seq[String]): Future[Either[Throwable, Seq[Thing]]] =
    Future.successful(Right(args.map(a => Thing("Apple" + a))))

  def main(args: Array[String]): Unit = {

    val itemsToDo: Map[String, Seq[String]] = Map("A" -> Seq("aaa"), "B" -> Seq("bbb"))

    val result: Future[Either[Throwable, Map[String, Seq[Thing]]]] = Future.sequence {
      itemsToDo.toSeq.map { case (hereTile, ecTiles) =>
        job(ecTiles).map(_.right.map((hereTile, _)))
      }
    }.map(eithers => {
      val acc: Either[Throwable, Map[String, Seq[Thing]]] = Right(Map())
      eithers.foldLeft(acc)((z, b) => z.right.flatMap(m => b.right.map(m + _)))
    })

    println(Await.result(result, 2 seconds))
  }

}
