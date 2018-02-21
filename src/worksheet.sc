import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

type Par[A] = ExecutorService => Future[A]

object Par {

  private case class SyncFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean) = false

    override def isCancelled = false

    override def isDone = true

    override def get(timeout: Long, unit: TimeUnit) = get
  }

  private case class StrictFuture[A](s: ExecutorService, a: Par[A])
    extends Future[A] {
    val f: Future[Future[A]] = s.submit(() => a(s))

    override def cancel(mayInterruptIfRunning: Boolean) =
      f.get.cancel(mayInterruptIfRunning)

    override def isCancelled = f.get().isCancelled

    override def isDone = f.get.isDone

    override def get(timeout: Long, unit: TimeUnit) =
      f.get(timeout, unit).get(timeout, unit)

    override def get() = {
      f.cancel(false)
      if (!f.isCancelled) f.get().get() else a(s).get()
    }
  }

  def unit[A](a: => A): Par[A] = _ => SyncFuture(a)

  // todo does is needed?
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def delay[A](a: => Par[A]): Par[A] = s => a(s)

  def fork[A](a: => Par[A]): Par[A] =
    s => StrictFuture(s, delay(a))


  def product[A, B](a: Par[A], b: Par[B]): Par[(A, B)] =
    s => {
      val p1 = a(s)
      val p2 = b(s)
      unit((p1.get(), p2.get()))(s)
    }

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    s => unit(f(a(s).get()))(s)

  def map_[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])
                   (f: (A, B) => C): Par[C] =
    map(product(a, b))(p => f(p._1, p._2))

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])
                      (f: (A, B, C) => D): Par[D] =
    map2(a, product(b, c))((l, r) => f(l, r._1, r._2))

  def map4[A, B, C, D, E]
  (a: Par[A], b: Par[B], c: Par[C], d: Par[D])
  (f: (A, B, C, D) => E): Par[E] =
    map2(a, product(b, product(c, d)))((l, r) =>
      f(l, r._1, r._2._1, r._2._2))

  def map5[A, B, C, D, E, F]
  (a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])
  (f: (A, B, C, D, E) => F): Par[F] =
    map2(a, product(b, product(c, product(d, e))))((l, r) =>
      f(l, r._1, r._2._1, r._2._2._1, r._2._2._2))

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] =
  //    fork(sequence(l.map(asyncF(f))))
    sequence(l.map(asyncF(f)))

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] =
    map(parMap(l)(a => (a, f(a))))(_.filter(_._2).map(_._1))

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map(l)(_.sorted)

  def parReduce[A](as: IndexedSeq[A], a0: A)
                  (f: (A, A) => A): Par[A] =
    as.length match {
      case 0 => unit(a0)
      case 1 => async(f(a0, as(0)))
      case _ =>
        val (l, r) = as.splitAt(as.length / 2)
        map2(fork(parReduce(l, a0)(f)),
          fork(parReduce(r, a0)(f)))(f)
    }

  def sum(as: IndexedSeq[Int]) = parReduce(as, 0)(_ + _)

  def max(as: IndexedSeq[Int]) =
    if (as.isEmpty) None
    else Some(parReduce(as, as(0))((a, b) =>
      if (a < b) b else a))

  def wordsCount(l: List[String]): Par[Int] =
    map(parMap(l)(_.split(" ").length))(_.sum)
}

//val s = Executors.newWorkStealingPool()
val s = Executors.newSingleThreadExecutor()
Par.map5(Par.async("A"), Par.async("B"),
  Par.async("C"), Par.async("D"),
  Par.async("E"))(_ + _ + _ + _ + _)(s).get()
Par.unit("a")(s).get()
Par.async("a" + "b")(s).get()
Par.map(Par.async("a" + "b"))(_ + " map")(s).get()
Par.asyncF[Int, String](a => "f:" + a)(2)(s).get()
Par.sequence(List(Par.async("h"),
  Par.async("u"), Par.async("y")))(s).get()
// fixme lines below: blocks with single thread executor
Par.sum(IndexedSeq(10, 20, 100, 3, -2))(s).get()
Par.max(IndexedSeq(10, 20, 100, 3, -2)).get(s).get()
Par.parMap(List(1, 2, 3))("map" + _)(s).get()
Par.wordsCount(List("abc ab abb", "bbs vv w", ""))(s).get()