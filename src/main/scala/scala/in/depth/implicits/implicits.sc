

/**
  * Checks Bar's companion object for implicit Bar instance
  * */
trait Bar
object Bar {
  implicit val x = new Bar { //this implicit is looked at second //easily overridable
    override def toString = "Companion Foo"
  }
}
def method(implicit bar : Bar) = println(bar)
method
/********************************************************/


/**
  * IMPLICIT SCOPE VIA TYPE PARAMETERS
  * */
trait Foo2
object Foo2 {
  implicit val list = List(new Foo2{})
}
implicitly[List[Foo2]]

/**
  * IMPLICTLY for Type Traits/Type Classes
  * */
trait BinaryFormat[T] {
  def asBinary(entity: T): Array[Byte]
}

//Like Play Json. Implicit format in Companion
trait Foo {}
object Foo {
  implicit lazy val binaryFormat = new BinaryFormat[Foo] {
    def asBinary(entity: Foo) =
      "serializedFoo".map(_.toByte).toArray
  }
}

object Formats {
  implicit lazy val binaryFormat = new BinaryFormat[String] {
    def asBinary(entity: String) =
      entity.map(_.toByte).toArray
  }
}

val foo = new Foo(){}

def toTest[A](value: A, format: BinaryFormat[A]): Array[Byte] = {
  format.asBinary(value)
}


val formatOfFoo = toTest[Foo](foo,implicitly[BinaryFormat[Foo]])

/**
  * IMPLICIT SCOPE VIA NESTING
  *
  * objects don't have companion objects.
  * Shove them in other objects and put implicits there too :-)
  * */

object Foo3 {
   object BarIn { override def toString = "BarIn" }
   implicit def b : BarIn.type = BarIn
}
implicitly[Foo3.BarIn.type]

/**
  * from package object
  * */
