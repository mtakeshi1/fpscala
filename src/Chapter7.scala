object Chapter7 {

  import java.util.concurrent.*

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = false

    override def get(timeout: Long, unit: TimeUnit) = get

    override def isCancelled: Boolean = false

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](la: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] {
      override def call(): A = la(es).get
    })
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val aa = a(es).get
    val bb = b(es).get
    UnitFuture(f(aa, bb))
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] = {
    map2(a, unit(()))((a, _) => f(a))
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = (es: ExecutorService) => {
    val aa: A = run(es)(a).get
    run(es)(f(aa))
  }

  object Exercise7_5 {

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case Nil => unit(List())
      case t :: rest => {
        val tail = sequence(rest)
        map2(t, tail)((el, list) => el :: list)
      }
    }

  }

  object Exercise7_6 {
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = as match {
      case Nil => unit(List())
      case h :: rest => {
        val maybeHead: Par[Option[A]] = lazyUnit(if (f(h)) Some(h) else None)
        val tail = parFilter(rest)(f)
        map2(maybeHead, tail)((maybe, list) => maybe.toList ::: list)
      }
    }
  }

  object Exercise7_11 {
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      flatMap(n)(index => choices(index))
    }

    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

  }

  object Exercise7_13 {

    def join[A](a: Par[Par[A]]): Par[A] = (es: ExecutorService) => {
      val value = run(es)(a).get
      run(es)(value)
    }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
      join(map(a)(f))
    }

  }

  object Exercise7_14 {
    def join[A](a: Par[Par[A]]): Par[A] = {
      flatMap(a)(para => para)
    }
  }


}
