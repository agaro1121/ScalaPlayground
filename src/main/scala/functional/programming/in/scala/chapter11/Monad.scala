package functional.programming.in.scala.chapter11

import scala.language.higherKinds

trait Monad[M[_]] {
  def unit[A](value: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /*
  * 11.3
  *
  * Could also be implemented with Folds
  * */
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case x :: xs => map2(x, sequence(xs))(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la match {
      case Nil => unit(Nil)
      case xs => sequence(xs.map(f))
    }

  /*
  * 11.4
  * */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    //    sequence(List.fill(n)(ma))
    //  traverse(List.fill(n)(ma))(x => x)
    n match {
      case 0 => unit(Nil)
      case x => map2(ma, replicateM(x - 1, ma))(_ :: _)
    }

  /*
  * 11.6
  * */
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(Nil: List[A])) {
      (a, lmb) =>
        map2(f(a), lmb)((b, as) => if (b) a :: as else as)
    }
}

object Monad {

  /*
  * 11.1
  * */
  val optionMonad = new Monad[Option] {
    override def unit[A](value: A): Option[A] = Some(value)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](value: A): Stream[A] = Stream(value)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](value: A): List[A] = List(value)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

}
