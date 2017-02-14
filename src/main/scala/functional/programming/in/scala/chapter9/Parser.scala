package functional.programming.in.scala.chapter9

import scala.language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

trait Parser[+A]
trait ParseError

trait Parsers[ParseError, Parser[+_]] {
  self ⇒

  def char(c: Char): Parser[Char]
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: ⇒ Parser[A]): Parser[A] //delegated to ParserOps

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A ⇒ Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: ⇒ Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map[C](f.tupled)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = ???

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] = p.map(List(_))

  /*
  * 9.6
  * */
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String] =
    flatMap(succeed(r))(regex)

  def wrap[A](p: => Parser[A]): Parser[A]
  /*
  * 9.7
  * Implement product and map2 in terms of flatMap.
  * */
  def productViaFlatMap[A, B](p: Parser[A], p2: ⇒ Parser[B]): Parser[(A, B)] =
    flatMap(p)(a ⇒ p2.map(b ⇒ (a, b)))
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: ⇒ Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a ⇒ p2.map(b ⇒ f(a, b)))

  /*
  * 9.8
  * map is no longer primitive. Express it in terms of flatMap and/or other combinators.
  * */
  def mapViaFlatMap[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a ⇒ succeed(f(a)))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: ⇒ Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: ⇒ Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    val numA: Parser[Int] = char('a').many.map(_.size)
    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: ⇒ Parser[B]): Parser[(A, B)] = self.product(p, p2)

    /*
    * 9.1
    * */
    def map2[B, C](p2: ⇒ Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
    val zeroOrMore: Parser[(Int, Int)] =
      char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

    /*
    * 9.3
    * */
    def manyViaMap2: Parser[List[A]] =
      p.map2(p.many)(_ :: _).or[List[A]](succeed(List()))

    /*
    * 9.4
    * */
    def listOfNViaMap2(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

  }

}
