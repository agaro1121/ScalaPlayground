package functional.programming.in.scala.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

//trait Par[A] {}
object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) ⇒ UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def isDone: Boolean = true
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  /*
  * 7.1
  * map2 - signature
  * */
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) ⇒ C): Par[C] =
    (es: ExecutorService) ⇒ {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map2WithTimeout[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) ⇒ C): Par[C] =
    (es: ExecutorService) ⇒ {
      val (af, time) = timeTaken(pa(es))
      val bf = pb(es)
      UnitFuture(f(af, bf.get(time, TimeUnit.MILLISECONDS)))
    }

  private def timeTaken[A](future: ⇒ Future[A]): (A, Long) = {
    val startTime = System.currentTimeMillis()
    val result = future.get()
    val endTime = System.currentTimeMillis()
    val timeTaken = endTime - startTime
    (result, timeTaken)
  }

  def run[A](a: Par[A])(s: ExecutorService): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] =
    es ⇒ es.submit(new Callable[A] { //blocks
      override def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /*
  * 7.4
  * */
  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) ⇒ lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  /*
  * 7.5
  * */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(Nil: List[A])) { (elem, acc) ⇒ map2(elem, acc)(_ :: _) }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  /*
  * 7.6
  * */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    /*as.foldRight(unit(Nil: List[A])) {
      (elem, acc) ⇒
        if(f(elem)) map(acc)(elem :: _)
        else acc
    }*/
    val pars: List[Par[List[A]]] =
      as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  //does computation when needed in main thread
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(cond)(es).get) t(es) else f(es)

  /*
  * 7.11
  * */
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es ⇒ choices(run(n)(es).get())(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN
    {
      es ⇒
        if (run(cond)(es).get) unit(0)(es) else unit(1)(es)
//      map(cond)(res ⇒ if(res) 0 else 1) //could also simply be this
    }(List(t, f))

  /*
  * 7.12
  * */
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es ⇒ choices(run(key)(es).get())(es)

  /*
  * 7.13
  * */
  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es ⇒ choices(run(pa)(es).get())(es)

  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = chooser(pa)(choices)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(bool ⇒ if (bool) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(n ⇒ choices(n))

  /*
  * 7.14
  * */
  def join[A](a: Par[Par[A]]): Par[A] =
    es ⇒ run(a)(es).get()(es)

  def flatMapViaJoin[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(choices))

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(a ⇒ a)

  def map2ViaFlatMapAndUnit[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) ⇒ C): Par[C] =
    flatMap(pa)(a ⇒ map2(unit(a), pb)(f))


}

