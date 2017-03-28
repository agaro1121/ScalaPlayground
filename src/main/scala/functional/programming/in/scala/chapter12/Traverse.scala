package functional.programming.in.scala.chapter12

import free.Functor
import functional.programming.in.scala.chapter10.Foldable
import functional.programming.in.scala.chapter11.Monad._
import functional.programming.in.scala.chapter11.Monad
import functional.programming.in.scala.chapter6.State

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

  /*
  * 12.16
  * */
  /*
    def reverse[A](fa: F[A]): F[A] =
       mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
    */

  /*
  * 12.17
  * */
  /*
     override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
        mapAccum(fa, z)((a, b) => ((), f(b, a)))._2
  * */

  /*
  * 12.18
  *
  *  fuse multiple traversals of a traversable structure
  *  This function will, given two functions f and g,
  *  traverse fa a single time, collecting the results of both functions at once.
  *
  * */
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  /*
  * 12.19
  * */
  /*def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      def traverse[G[_] : Applicative, A, B](fa: F[G[A]])(f: A => G[B]): G[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))

      override def map[A, B](a: F[G[A]])(fn: A => B): F[G[B]] = ???
    }*/

  /*
  * 12.20
  * */
  /*def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({ type f[x] = F[G[x]] })#f] =
    new Monad[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](value: A): F[G[A]] = F.unit(G.unit(value))
      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(ma) { ga =>
          F.map(T.traverse(ga)(f))(G.join)
        }

    }*/

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