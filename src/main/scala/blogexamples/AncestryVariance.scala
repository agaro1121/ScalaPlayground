package blogexamples

trait Grandparent                     { override def toString = "Grandparent" }
trait Parent      extends Grandparent { override def toString = "Parent" }
trait Child       extends Parent      { override def toString = "Child" }

// CoVariance
trait AcceptsAnyTypeAOrBelow[+A] {
  def printType[B >: A](value: B): Unit = println(identity(value))
}

// ContraVariance
trait AcceptsAnyTypeAOrAbove[-A] {
  def printType(value: A): Unit = println(identity(value))
}

object Methods {

  val grandparent = new Grandparent {}
  val parent = new Parent {}
  val child = new Child {}

  def invariance(value: Parent): Unit = {
    println("only accepts parent and it's subtypes = "+value)
  }

  def covariance[T <: Parent](value: T): Unit = {
    println("only accepts parent and child = "+ identity(value))
  }

  def contravariance[T >: Parent](value: T): Unit = {
    println("only accepts parent and grandparent = "+value)
  }

}

object CovariantMethodsTester extends App {

  import Methods._

/*
  invariance(grandparent)
  compile error - grandparent is NOT a parent
  BUT parent is a grandparent, just for completion
 */
//  invariance(parent) //parent is a parent
//  invariance(child) //child is a parent

  /*
   *   exception at compile time
   * */
//covariance(grandparent)
  covariance(parent)
  covariance(child)
}

object CovariantTraitTester extends App {
  val covariantParentValidator = new AcceptsAnyTypeAOrBelow[Parent] {}

  val grandparent = new Grandparent {}
  val parent = new Parent {}
  val child = new Child {}


  /*
   *  errors during compilation
   * */
//  covariantParentValidator.printType(grandparent)
  covariantParentValidator.printType(parent)
  covariantParentValidator.printType(child)
}



object ContravariantTraitTester extends App {
  val contravariantValidator = new AcceptsAnyTypeAOrAbove[Parent] {}

  val grandparent: Grandparent = new Grandparent {}
  val parent: Parent = new Parent {}
  val child: Child = new Child {}


//  contravariantValidator.printType(grandparent)
  contravariantValidator.printType(parent)
  contravariantValidator.printType(child)

}
