package functional.programming.in.scala.chapter12

import free.Functor

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {

  //primitives
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](value: => A): F[A]

  //derived
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /*
  * 12.1
  * */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
//    traverse(fas)(fa => fa)
    fas.foldRight(unit(Nil: List[A]))((e, acc) => map2(e,acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa, fb)((_,_))

}
