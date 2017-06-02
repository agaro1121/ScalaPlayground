package books.underscoreio.advancedscala

import scala.util.Try

trait Codec[A] {
  def encode(value: A): String
  def decode(value: String): Option[A]

  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    val self = this
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))
        override def decode(value: String): Option[B] = self.decode(value).map(dec)
      }
    }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)

  implicit val intCodec = new Codec[Int] {
    override def encode(value: Int): String = value.toString
    override def decode(value: String): Option[Int] = Try(value.toInt).toOption
  }

  implicit val stringCodec = new Codec[String] {
    override def encode(value: String): String = value
    override def decode(value: String): Option[String] = Some(value)
  }

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = {
    c.imap[Box[A]](Box(_), _.value)
  }

}



object InvarantFunctors extends App {
  import Codec._

  println(encode(Box(123)))
  println(decode[Box[Int]]("123"))

}
