package functional.programming.in.scala.chapter12

import free.Functor

import scala.language.higherKinds
import scala.language.reflectiveCalls
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

  /*
  * 12.8
  * */
  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  /*
  * 12.9
  * */
  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fa, fb)(G.map2(_, _)(f))
    }
  }

}

object Applicative {

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] =
      a zip b map f.tupled

  }

  import functional.programming.in.scala.chapter11._, Monad._

  /*
  * 12.5
  * */
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = new Monad[({ type f[x] = Either[E, x] })#f] {

    override def flatMap[A, B](ma: Either[E, A])(f: A ⇒ Either[E, B]): Either[E, B] = {
      ma match {
        case Left(e) ⇒ Left(e)
        case Right(v) ⇒ f(v)
      }
    }

    override def unit[A](value: A) = Right(value)
  }

  /*
  * 12.6
  * */
  def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      override def unit[A](a: ⇒ A): Validation[Nothing, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) ⇒ C): Validation[E, C] = {
        (fa, fb) match {
          case (Success(v), Success(v2)) ⇒ Success(f(v, v2))
          case (Failure(h, t), Failure(h2, t2)) ⇒ Failure(h, (t :+ h2) ++ t2)
          case (_, f @ Failure(_, _)) ⇒ f
          case (f @ Failure(_, _), _) ⇒ f
        }
      }
    }

}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]
