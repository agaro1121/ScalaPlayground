package scala.in.depth

/**
  * Created by Hierro on 5/8/16.
  */
trait CanDo[A] {
  def doStuff[A](a: A): Unit
}

trait Stuffs {
  def stuff(): Unit

  implicit lazy val strings = new CanDo[String] {
    def doStuff[String](a: String) = println("strings !!! arg=" + a)
  }

  implicit lazy val ints = new CanDo[Int] {
    def doStuff[Int](a: Int) = println("ints !!! arg=" + a)
  }

}

object Stuffs {
  def stuff() = println("stuff to link these")

  implicit lazy val doubles = new CanDo[Double] {
    def doStuff[Double](a: Double) = println("doubles !!! arg=" + a)
  }
}


object Tester extends App with Stuffs {

  override def stuff(): Unit = println("stuff from Tester")

  def testAnything[A: CanDo](value: A)(implicit stuffer: CanDo[A]) = stuffer.doStuff(value) //the long way

  //implicitly[CanDo].doStuff(value)
  def testAnything2[A: CanDo](value: A, stuffer: CanDo[A]) = stuffer.doStuff(value) //implicitly[CanDo].doStuff(value)

  testAnything[String]("Hello")
  testAnything[Int](5)
  testAnything2[String]("Hello", implicitly[CanDo[String]])
  testAnything2[Int](5, implicitly[CanDo[Int]])


//  testAnything2[Double](5.5, implicitly[CanDo[Double]]) //trying to get this from companion object


}

class Echo[A](){
  def echo(a : A ) = "echo=" + a.toString
  override def toString: String = "Echo" + super.toString
}

object ContextBoundTester extends App {
  implicit val intEcho = new Echo[Int]()

  def g[A : Echo](a : A) = println(a) //eve if we don't use echo here, it complains at runtime that it's missing
  def g2[A : Echo](a : A) = println(implicitly[Echo[A]].echo(a))
  g(3)
  g2(3)
}