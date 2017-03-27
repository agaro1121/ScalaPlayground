package functional.programming.in.scala.chapter12

import free.Functor
import functional.programming.in.scala.chapter11.Monad._

import scala.language.higherKinds

trait Traverse[F[_]] extends Functor[F] {

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A
  implicit val idM = idMonad

  /*
  * 12.14
  * */
//  def map[A, B](fa: F[A])(f: A => B): F[B] =
//    traverse[Id, A, B](fa)(f)(idM) <- this should work but Monad doesn't extend Applicative


}

object Traverse {
  /*
  * 12.13
  * Write Traverse instances for List, Option, and Tree.
  * */
  case class Tree[+A](head: A, tail: List[Tree[A]])

  def listTraverse = new Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val appG = implicitly[Applicative[G]]
      val zero = appG.unit(Nil: List[B])
      fa.foldRight(zero) {
        (elem, acc) =>
          appG.map2(f(elem), acc)(_ :: _)
      }
    }

    override def map[A, B](a: List[A])(fn: (A) => B): List[B] = a map fn
  }

  def optionTraverse = new Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa match {
        case None => implicitly[Applicative[G]].unit(None)
        case Some(value) => implicitly[Applicative[G]].map(f(value))(Some(_))
      }

    override def map[A, B](fa: Option[A])(fn: (A) => B): Option[B] = fa map fn
  }

  def treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(fn: (A) => B): Tree[B] = {
      Tree[B](fn(fa.head), listTraverse.map(fa.tail)(a => treeTraverse.map(a)(fn)))
    }

    override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val appG = implicitly[Applicative[G]]
      appG.map2(f(fa.head), listTraverse.traverse(fa.tail)(a => traverse(a)(f)))(Tree(_, _))
    }
  }

}