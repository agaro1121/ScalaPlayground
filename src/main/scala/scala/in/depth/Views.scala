package scala.in.depth

/**
  * Created by Hierro on 5/8/16.
  */

class SupaString(val s : String) {
  def makeSuper(): Unit = println("Superrrrrr " + s)
  def makeSuper2() = "Superrrrrr " + s

  override def toString: String = "Super " + s
}
object SupaString {
  implicit def wrap(s: String) = new SupaString(s)
}

object Views extends App {

  def hello[String <% SupaString](x : String) = x.makeSuper()
  def hello2[String <% SupaString](x : String) = x.makeSuper2()

  def helloExplicit[String](x: String)(implicit $ev0: String => SupaString) = x.makeSuper()

  def doIt[String <% SupaString](x : String) = x //using toString from String not SupaString

  hello("Saluton")
  helloExplicit("Saluton Explicit")
  println(hello2("Saluton2"))
  println(doIt("Saluton"))


}
