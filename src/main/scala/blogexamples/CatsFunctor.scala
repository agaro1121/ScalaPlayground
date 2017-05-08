package blogexamples

import cats.laws.FunctorLaws
import cats.laws.discipline.FunctorTests
import org.scalacheck.Gen
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

object CatsFunctor extends App {

  implicit val treeFunctor = new cats.Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] =
      fa match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
  }

  implicit val treeEq = new cats.Eq[Tree[Int]] {
    override def eqv(x: Tree[Int], y: Tree[Int]): Boolean =
      (x, y) match {
        case (Leaf(a), Leaf(b)) => a == b
        case (Leaf(_), _) => false
        case (Branch(l, r), Branch(l2, r2)) => eqv(l, l2) && eqv(r, r2)
        case (Branch(_, _), _) => false
      }
  }

  implicit val treeFunctorLaws = FunctorLaws[Tree]

  val genLeaf = for { v <- arbitrary[Int] } yield Leaf(v)

  val genBranch = for {
    _ <- arbitrary[Int]
    l <- genTree
    r <- genTree
  } yield Branch(l, r)

  def genTree: Gen[Tree[Int]] = oneOf(genLeaf, genBranch)

  implicit val arb: Arbitrary[Tree[Int]] = Arbitrary[Tree[Int]](genTree)

  val rs = FunctorTests[Tree].functor[Int, Int, Int]

  rs.all.check()


}