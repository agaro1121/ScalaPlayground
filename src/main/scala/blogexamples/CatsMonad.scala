package blogexamples
import Tree.leaf
import cats.laws.MonadLaws
import cats.laws.discipline.MonadTests
import cats.implicits._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

object CatsMonad extends App {


  implicit val treeMonad = new cats.Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(a) => f(a)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      flatMap(f(a)) {
        a =>
          a match {
            case Left(aa) => tailRecM(aa)(f)
            case Right(b) => leaf(b)
          }
      }
    }

    override def pure[A](x: A): Tree[A] = Tree.leaf(x)
  }

  implicit val treeTripleEq = new cats.Eq[Tree[(Int, Int, Int)]] {
    override def eqv(x: Tree[(Int, Int, Int)], y: Tree[(Int, Int, Int)]): Boolean =
      (x, y) match {
        case (Leaf(a), Leaf(b)) => a == b
        case (Leaf(_), _) => false
        case (Branch(l, r), Branch(l2, r2)) => eqv(l, l2) && eqv(r, r2)
        case (Branch(_, _), _) => false
      }
  }

  val genLeafMonad: Gen[Leaf[Int => Int]] =  for {v <- arbitrary[Int => Int] } yield Leaf(v)

  val genBranchMonad: Gen[Branch[Int => Int]] = for {
    _ <- arbitrary[Int => Int]
    l <- genTreeMonad
    r <- genTreeMonad
  } yield Branch(l, r)

  def genTreeMonad: Gen[Tree[Int => Int]] = genLeafMonad//oneOf(genLeafMonad, genBranchMonad)

  implicit val arbMonad: Arbitrary[Tree[Int => Int]] = Arbitrary[Tree[Int => Int]](genTreeMonad)

  val monadLaws = MonadLaws[Tree]

  import CatsFunctor.{treeEq,arb}
  val rsMonad = MonadTests[Tree].monad[Int, Int, Int]

  println(rsMonad.all)

  val tree: Tree[Int] = Branch(Branch(Leaf(4), Branch(Leaf(5), Leaf(6))),Leaf(8))

  println(tree.flatMap(a => leaf(a + 1)))

}

