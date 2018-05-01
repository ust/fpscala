import java.util.concurrent.{ExecutorService, Executors, Future}

import parallelism._

type Par[A] = ExecutorService => Future[A]


val s = Executors.newSingleThreadExecutor()
//val s = Executors.newWorkStealingPool()
Par.join(Par.async(Par.async("hui")))(s).get()
Par.choiceN(Par.async(2))(List(Par.async("hui"),
  Par.async("bolt"), Par.async("shnyaga")))(s).get()
Par.choice(Par.async(false))(Par.async("hui"),
  Par.async("bolt"))(s).get()
Par.map5(Par.async("A"), Par.async("B"),
  Par.async("C"), Par.async("D"),
  Par.async("E"))(_ + _ + _ + _ + _)(s).get()
Par.unit("a")(s).get()
Par.async("a" + "b")(s).get()
Par.map(Par.async("a" + "b"))(_ + " map")(s).get()
Par.asyncF[Int, String](a => "f:" + a)(2)(s).get()
Par.sequence(List(Par.async("h"),
  Par.async("u"), Par.async("y")))(s).get()
Par.sum(IndexedSeq(10, 20, 100, 3, -2))(s).get()
Par.max(IndexedSeq(10, 20, 100, 3, -2)).get(s).get()
Par.parMap(List(1, 2, 3))("map" + _)(s).get()
Par.wordsCount(List("abc ab abb", "bbs vv w", ""))(s).get()