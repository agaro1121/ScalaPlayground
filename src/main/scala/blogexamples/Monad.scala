package blogexamples

import blogexamples.Tree.{branch, leaf}

trait Monad[M[_]] {
  def flatMap[A,B](ma: M[A], f: A => M[B]): M[B]
  def pure[A](a: A): M[A]
}

object Monad {

  case class MonadOps[M[_], A](ma: M[A]) {
    def flatMap[B](f: A => M[B])(implicit monad: Monad[M]): M[B] = monad.flatMap(ma, f)
//    def pure(a: A)(implicit monad: Monad[M]): M[A] = monad.pure(a) //TODO: don't need this?
  }
  implicit def toMonadOps[M[_], A](ma: M[A]): MonadOps[M,A] = MonadOps(ma)

  implicit val treeMonad = new Monad[Tree]{
    override def flatMap[A, B](ma: Tree[A], f: (A) => Tree[B]): Tree[B] = {
      ma match {
        case Leaf(a) => f(a)
        case Branch(l, r) => Branch(flatMap(l, f), flatMap(r, f))
      }
    }
    override def pure[A](a: A): Tree[A] = Tree.leaf(a)
  }

  def apply[M[_]](implicit monad: Monad[M]) = implicitly[Monad[M]]

}


object MonadTester extends App {
  import Monad._

  val tree = branch(
    branch(leaf(4), branch(leaf(5), leaf(6))),
    leaf(8)
  )

  println(implicitly[Monad[Tree]].pure(5))

  println(Monad[Tree].pure(45))

  println(tree.flatMap(a => leaf(a + 1)))

}