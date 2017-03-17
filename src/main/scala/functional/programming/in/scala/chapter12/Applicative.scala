package functional.programming.in.scala.chapter12

import free.Functor

import scala.language.higherKinds
import scala.util.Either.RightProjection

trait Applicative[F[_]] extends Functor[F] {

  //primitives
  def unit[A](a: => A): F[A]

  //derived
  //  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  /*
  * 12.2
  * */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    //map2(fab, fa)(_(_))
    map2[A, A => B, B](fa, fab)((a, f) => f(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply[A, B](unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    //    apply[B, C](map(fa)(f.curried))(fb)
    apply(apply(unit(f.curried))(fa))(fb)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  /*
  * 12.1
  * */
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    //    traverse(fas)(fa => fa)
    fas.foldRight(unit(Nil: List[A]))((e, acc) => map2(e, acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  /*
  * 12.4
  * */
  def map3[A, B, C, D](fa: F[A], fb: F[B],
    fc: F[C])(f: (A, B, C) => D): F[D] =
    //    apply[C,D](map2(fa, fb)((a,b) => f.curried(a)(b)))(fc)
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C],
    fd: F[D])(f: (A, B, C, D) => E): F[E] =
    //    apply[D,E](map3(fa,fb,fc)((a,b,c) => f.curried(a)(b)(c)))(fd)
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

}

object Applicative {

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

  }

  import functional.programming.in.scala.chapter11._, Monad._

  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = new Monad[({ type f[x] = Either[E, x] })#f] {

    override def flatMap[A, B](ma: Either[E, A])(f: A ⇒ Either[E, B]): Either[E, B] = {
      ma match {
        case Left(e) ⇒ Left(e)
        case Right(v) ⇒ f(v)
      }
    }

    override def unit[A](value: A) = Right(value)
  }

}