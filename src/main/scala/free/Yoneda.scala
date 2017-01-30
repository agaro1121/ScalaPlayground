package free

import scala.language.higherKinds

trait Yoneda[F[_], A] {
  def map[B](f: A => B): F[B]
}
object Yoneda {

  def toYo[F[_], A](fa: F[A])(implicit functor: Functor[F]) = new Yoneda[F, A] {
    def map[B](f: A => B): F[B] = functor.map(fa)(f)
  }

  def froYo[F[_], A](yo: Yoneda[F, A]): F[A] = yo.map(a => a)
}

trait CoYoneda[F[_], A] {
  type I
  def f: I => A
  def fi: F[I]
}
object CoYoneda {

  def toCoYo[F[_], A](fa: F[A]) = new CoYoneda[F, A] {
    type I = A
    val f = (a: A) => a
    val fi = fa
  }

  def froCoYo[F[_], A](yo: CoYoneda[F, A])(implicit functor: Functor[F]): F[A] =
    functor.map(yo.fi)(yo.f)
}