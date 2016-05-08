package universal.`trait`.value.classes


/**
  * Created by Hierro on 4/25/16.
  */
object Example {

  /** Source - Scala Cookbook
    *
    * A ''universal trait'' is a trait that extends `Any`, only has `def`s as members, and does no initialization.
    *
    * The main use case for universal traits is to allow basic inheritance of methods for [[scala.AnyVal value classes]].
    * For example,
    *
    * {{{
    *     trait Printable extends Any {
    *       def print(): Unit = println(this)
    *     }
    *     class Wrapper(val underlying: Int) extends AnyVal with Printable
    *
    *     val w = new Wrapper(3)
    *     w.print()
    * }}}
    *
    * */

  /** Source - http://docs.scala-lang.org/overviews/core/value-classes.html
    *
    *
    * Value classes are a new mechanism in Scala to avoid allocating runtime
    * objects. This is accomplished through the definition of new AnyVal subclasses
    *
    * */

  class Wrapper(val underlying: Int) extends AnyVal {
    //can ONLY extend Universal Traits (Any, AnyVal). See example below
    //At compile time - Wrapper
    //at runtime = int
    def foo: Wrapper = new Wrapper(underlying * 19) //can only have defs
    // NO vals,vars,lazy vals, inner classes/objects/traits
  }

  trait Printable extends Any {
    def print(): Unit = println(this)
  }

  class Wrapper2(val underlying: Int) extends AnyVal with Printable

  //extends Any and AnyVal just fine

  /**
    * Use Cases:
    *
    * 1. Extension Methods - combine them with implicit classes (SIP-13) for allocation-free extension methods
    * Using an implicit class provides a more convenient syntax for defining extension methods, while value
    * classes remove the runtime overhead
    * - BASICALLY -
    * Allows you to create implicit extension methods equivalent to that of Static Methods(no instance required)
    *
    * 2. Correctness - get the type safety of a data type without the runtime allocation overhead
    * Example:
    **/

  /**
    * 2. Correctness - get the type safety of a data type without the runtime allocation overhead
    * Example:
    */
  /*class Meter(val value: Double) extends AnyVal {
    def +(m: Meter): Meter = new Meter(value + m.value)
  }*/

  /**
    * Only the underlying Double is used below.
    * The Meter class is never instantiated.
    *
    * So this provides the runtime type safety at compile time
    * without the overhead of instantiating objects
    **/
  val x = new Meter(3.4)
  val y = new Meter(4.3)
  val z = x + y

  /**
    * When Allocation Is Necessary
    *
    * JVM does not support value classes,
    * Scala sometimes needs to actually instantiate a valu
    * e class
    *
    * Scenarios Below:
    */

  /**
    * 1. a value class is treated as another type.
    */
  trait Distance extends Any

  case class Meter(val value: Double) extends AnyVal with Distance {
    def +(m: Meter): Meter = new Meter(value + m.value)
  }

  //This requires allocation (Polymorphic I guess?)
  def add(a: Distance, b: Distance): Distance = ???

  add(Meter(3.4), Meter(4.3))

  //This WOULD NOT require allocation since it's not being treated as as differnt type
  def add(a: Meter, b: Meter): Meter = ???

  /** Another Example for Completeness */
  def identity[T](t: T): T = t

  identity(Meter(5.0))
  //Passed in T but trying to use Meter here

  /**
    * 2. a value class is assigned to an array
    * Even if the array is an array of Value Classes, it still needs instances
    */
  val m = Meter(5.0)
  val array = Array[Meter](m)

  /** 3. doing runtime type tests, such as pattern matching.
    * Another scenario is asInstanceOf[T]
    * */
  case class P(val i: Int) extends AnyVal

  val p = new P(3)
  p match {
    // new P instantiated here
    case P(3) => println("Matched 3")
    case P(x) => println("Not 3")
  }

}

//can stand alone or be inside another Object or Trait NOT A CLASS
class Wrapper2(val underlying: Int) extends AnyVal

