package blogexamples

import scala.language.{higherKinds, implicitConversions}

trait Functor[F[_]] { //
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  case class FunctorOps[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] = functor.map(fa)(f)
  }
  implicit def toFunctorOps[F[_], A](fa: F[A]): FunctorOps[F, A] = FunctorOps(fa)
}

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](a: A): Tree[A] = Leaf(a)
}

object FunctorTester extends App {
  import Functor._
  import Tree._

  val tree = branch(
    branch(leaf(4), branch(leaf(5), leaf(6))),
    leaf(8)
  )

  println(tree.map(_ + 1))
  println(tree.map(n => s"n=$n"))


  val f: Int => Int = i => i + 1
  val g: Int => Int = i => i - 1

  // laws
  println(tree.map(x => x))
  println(tree.map(f).map(g) == tree.map(f.andThen(g)))

}