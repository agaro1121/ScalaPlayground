package scala.in.depth.types

/**
  * Created by Hierro on 5/8/16.
  */
object Constraints extends App {

  implicit object StringNumeric extends Numeric[String] {
    override def plus(x: String, y: String): String = x + y

    override def toDouble(x: String): Double = ???

    override def toFloat(x: String): Float = ???

    override def toInt(x: String): Int = ???

    override def negate(x: String): String = ???

    override def fromInt(x: Int): String = ???

    override def toLong(x: String): Long = ???

    override def times(x: String, y: String): String = ???

    override def minus(x: String, y: String): String = ???

    override def compare(x: String, y: String): Int = ???

    override def zero: String = ""
  }

  /**
    * def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)
    * above does implicit lookup of Numeric type and enforces
    * that only subtypes of Numeric Can use Numeric's functions
    *
    * Since String is not a subtype Numeric supports, you get an error
    * <console>:16: error: could not find implicit value for parameter num: Numeric[String]
    *  List("One", "Two", "Three").sum
    *
    *  With the above implementation and override, this works fine
    * */

  println(List("One", "Two", "Three").sum) //OneTwoThree
  println(List("One", "Two", "Three").sum(StringNumeric)) //OneTwoThree

}

object Constraints2 extends App {

  def peek[A,C](col: C)(implicit ev: C =:= List[Int]) = (col.head,col) //has to be exact match
  def peek2[A,C](col: C)(implicit ev: C =:= List[String]) = (col.head,col)
  def peek3[A,C](col: C)(implicit ev: C =:= Traversable[Int]) = (col.head,col) //has to be exact match

  println(peek(List(1, 2, 3)))
  println(peek2(List("1", "2", "3")))
//  println(peek3(List(1, 2, 3))) //Does not work


}

object Constraints3 extends App {
  def peek[C, A](col: C)(implicit ev: C <:< Traversable[A]) = (col.head, col) //has to be subtype of Traversable[A]

  println(peek(List(1, 2, 3)))
}