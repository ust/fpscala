import java.util.concurrent.{ExecutorService, Future, TimeUnit}

type Par[A] = ExecutorService => Future[A]


object Par {

  private case class SyncFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean) = false

    override def isCancelled = false

    override def isDone = true

    override def get(timeout: Long, unit: TimeUnit) = get
  }

  def unit[A](a: => A): Par[A] = _ => SyncFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)


  def fork[A](a: => Par[A]): Par[A] =
    s => s.submit(() => a(s).get())

  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    s => {
      val p1 = fa(s)
      val p2 = fb(s)
      unit((p1.get(), p2.get()))(s)
    }

  def map[A, B](fa: Par[A])(f: A => B): Par[B] = ???

  def map_[A, B](fa: Par[A])(f: A => B): Par[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def map2[A, B, C](a: Par[A], b: Par[B])
                   (f: (A, B) => C): Par[C] =
    s => {
      val p1 = a(s)
      val p2 = b(s)
      unit(f(p1.get(), p2.get()))(s)
    }

  def async[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a))

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)
}

