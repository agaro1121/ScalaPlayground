package functional.programming.in.scala.chapter13

import functional.programming.in.scala.chapter11.Monad
import functional.programming.in.scala.chapter7.Par.Par

trait Free[F[_], A] {

  /*
  * 13.1
  * */
  def map[B](f: A => B): Free[F, B] = flatMap(fa => Return(f(fa)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

}
case class Suspend[F[_], A](resume: Free[F, A]) extends Free[F, A]
case class Return[F[_], A](value: A) extends Free[F, A]
case class FlatMap[F[_], A, B](free: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
object Free {
  type TailRec[A] = Free[Function0,A]
  type Async[A] = Free[Par,A]

  /*
  * 13.1
  * */
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] =
    new Monad[({type f[a] = Free[F, a]})#f] {
      override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma flatMap f
      override def unit[A](value: A): Free[F, A] = Return(value)
    }
}