package functional.programming.in.scala.chapter7_2

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

import functional.programming.in.scala.chapter7_2.Par2.Par2

case class UnitFuture[A](get: A) extends Future[A] {
  override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

  override def isCancelled: Boolean = false

  override def isDone: Boolean = true

  override def get(timeout: Long, unit: TimeUnit): A = get
}

case class Map2FutureWithTimeout[A, B, C](a: Future[A],
                                          b: Future[B],
                                          f: (A, B) => C) extends Future[C] {
  @volatile private var cache: Option[C] = None

  override def cancel(mayInterruptIfRunning: Boolean): Boolean =
    a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

  override def isCancelled: Boolean = a.isCancelled || b.isCancelled

  override def isDone: Boolean = cache.isDefined

  override def get(): C = compute(Long.MaxValue)

  override def get(timeout: Long, unit: TimeUnit): C =
    compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

  private def compute(time: Long): C = cache match {
    case Some(c) => c
    case None =>
      val start = System.nanoTime
      val ar = a.get(time, TimeUnit.NANOSECONDS)
      val stop = System.nanoTime
      val aTime = stop - start
      val br = b.get(time - aTime, TimeUnit.NANOSECONDS)
      val ret = f(ar, br)
      cache = Some(ret)
      ret
  }

}

object Par2 {
  type Par2[T] = ExecutorService => Future[T]


  def unit[A](a: A): Par2[A] = _ => UnitFuture(a)

  def map2[A, B, C](a: Par2[A], b: Par2[B])(f: (A, B) => C): Par2[C] =
    es => Map2FutureWithTimeout(a(es), b(es), f)

  def map[A, B](pa: Par2[A])(f: A => B): Par2[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par2[A]): Par2[A] = es => {
    es.submit(() => a(es).get)
  }

  def lazyUnit[A](a: => A): Par2[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par2[A]): Future[A] = a(es)

  def asyncF[A, B](f: A => B): A => Par2[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par2[A]]): Par2[List[A]] = {
    ps.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par2[List[B]] =
    fork {
      val fbs: List[Par2[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

  def parFilter[A](as: List[A])(f: A => Boolean): Par2[List[A]] = {
    val pars: List[Par2[List[A]]] =
      as map (asyncF((a: A) => if (f(a)) List(a) else List()))

    map(sequence(pars))(_.flatten)
  }

  def map3[A, B, C, D](a: Par2[A], b: Par2[B], c: Par2[C])(f: (A, B, C) => D): Par2[D] = {
    map2(c, map2(a, b)((x, y) => f.curried(x)(y)))((c, func) => func(c))
  }

  def map4[A, B, C, D, E](a: Par2[A], b: Par2[B], c: Par2[C], d: Par2[D])(f: (A, B, C, D) => E): Par2[E] = {
    map2(d, map3(a, b, c)((x, y, z) => f.curried(x)(y)(z)))((d, func) => func(d))
  }

  def map5[A, B, C, D, E, F](a: Par2[A], b: Par2[B], c: Par2[C], d: Par2[D], e: Par2[E])
                            (f: (A, B, C, D, E) => F): Par2[F] = {
    map2(e, map4(a, b, c, d)((w, x, y, z) => f.curried(w)(x)(y)(z)))((e, func) => func(e))
  }

}

object Par2Implicits {

  implicit class Par2Ops[A](val a: Par2[A]) extends AnyVal {
    def map[B](f: A => B): Par2[B] = Par2.map(a)(f)
    def map2[B, C](b: Par2[B])(f: (A, B) => C): Par2[C] = Par2.map2(a, b)(f)
    def map3[B, C, D](b: Par2[B], c: Par2[C])(f: (A, B, C) => D): Par2[D] = Par2.map3(a, b, c)(f)
    def map4[B, C, D, E](b: Par2[B], c: Par2[C], d: Par2[D])(f: (A, B, C, D) => E): Par2[E] = Par2.map4(a, b, c, d)(f)
    def map5[B, C, D, E, F](b: Par2[B], c: Par2[C], d: Par2[D], e: Par2[E])(f: (A, B, C, D, E) => F): Par2[F] = Par2.map5(a, b, c, d, e)(f)
  }

}

object Par2Main extends App {

  import Par2Implicits._

  implicit val es = Executors.newWorkStealingPool(4)

  val par = Par2.unit(1)
  val par2 = Par2.unit(2)


  println(par.map2(par2)(_ + _)(es).get)
  println(par.map(_ + 1)(es).get)
}