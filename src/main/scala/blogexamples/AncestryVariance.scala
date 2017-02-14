package blogexamples

trait Grandparent { override def toString: String = "Grandparent" }
trait Parent extends Grandparent { override def toString: String = "Parent" }
trait Child extends Parent { override def toString: String = "Child" }

trait ParentAndBelow[+A] { override def toString: String = "ParentAndBelow" }
trait ParentAndAbove[-A] { override def toString: String = "ParentAndAbove" }
trait ParentOnly[A] { override def toString: String = "ParentOnly" }


object ContraVarianceTraitTester extends App {

  val grandparent: Grandparent = new Grandparent {}
  val parent: Parent = new Parent {}
  val child: Child = new Child {}

  def tester(x: ParentAndAbove[Parent]): Unit = println(identity(x))

  val v = new ParentAndAbove[Grandparent] {}
  val v2 = new ParentAndAbove[Parent] {}
  val v3 = new ParentAndAbove[Child] {}

  tester(v)
  tester(v2)
  //  tester(v3) //compile error
}

object CoVarianceTraitTester extends App {

  val grandparent: Grandparent = new Grandparent {}
  val parent: Parent = new Parent {}
  val child: Child = new Child {}

  def tester(x: ParentAndBelow[Parent]): Unit = println(identity(x))

  val v = new ParentAndBelow[Grandparent] {}
  val v2 = new ParentAndBelow[Parent] {}
  val v3 = new ParentAndBelow[Child] {}

  //  tester(v) //compile error
  tester(v2)
  tester(v3)
}

object InVarianceTraitTester extends App {

  val grandparent: Grandparent = new Grandparent {}
  val parent: Parent = new Parent {}
  val child: Child = new Child {}

  def tester(x: ParentOnly[Parent]): Unit = println(identity(x))

  val v = new ParentOnly[Grandparent] {}
  val v2 = new ParentOnly[Parent] {}
  val v3 = new ParentOnly[Child] {}

  //  tester(v) //compile error
  tester(v2)
  //  tester(v3) //compile error
}
