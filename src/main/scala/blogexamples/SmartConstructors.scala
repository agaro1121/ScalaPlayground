package blogexamples.smartconstructors

sealed trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class BatmanTree[A](tree: Tree[A])  {
  def isBatmanTree: Boolean = true
}

object Tree {
  implicit def toBatmanTree[A](tree: Tree[A]): BatmanTree[A] = BatmanTree(tree)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](a: A): Tree[A] = Leaf(a)

}


object SmartConstructorsTester extends App {
  import Tree._

  val leafNotSmart: Leaf[Int] = Leaf(1)
  val branchNotSmart: Branch[Int] = Branch(Leaf(1), Leaf(2))

  println(leafNotSmart.isBatmanTree)

  val leafWithSmartConstructors = leaf(1)
  val branchWithSmartConstructors = branch(leaf(1), leaf(2))

  println(branchWithSmartConstructors.isBatmanTree)
  println(leafWithSmartConstructors.isBatmanTree)



}