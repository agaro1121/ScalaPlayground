package scala.in.depth

import scala.collection.immutable.HashMap
import scala.collection.immutable.HashMap.HashTrieMap

/**
  * Created by Hierro on 5/7/16.
  */
object Bounds extends App {


  val a = new Z {
    type B = E
  }

  println(a.foo(new G()))

}

class Z {
  type B >: F

  def foo(a: B) = a
}

trait A
class D extends A {
  override def toString: String = "D"
}
class E extends D {
  override def toString: String = "E"
}
class F extends E {
  override def toString: String = "F"
}
class G extends F {
  override def toString: String = "G"
}