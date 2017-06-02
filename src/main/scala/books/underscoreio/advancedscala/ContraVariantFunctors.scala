package books.underscoreio.advancedscala

final case class Box[A](value: A)

trait Printable[A] {
  def format(value: A): String

  /*
  * This says that if A is Printable,
  * and we can transform B into A,
  * then B is also Printable.
  * */
  def contramap[B](f: B => A): Printable[B] = {
    val self = this //or could have put `self` at top of trait
    new Printable[B] {
      override def format(value: B): String = self.format(f(value))
    }
  }

}

object Printable {
  implicit val stringPrintable =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""


    }

  implicit val booleanPrintable =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  /*
  * Basically if you have Printable[Boolean],
  * you get a Printable[Box[Boolean]] for free.
  * Genius...
  * */
  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)

}

object ContraVariantFunctors extends App {

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  println(format("hello"))
  println(format(true))

  println(format(Box("hello world")))
  println(format(Box(true)))

}
