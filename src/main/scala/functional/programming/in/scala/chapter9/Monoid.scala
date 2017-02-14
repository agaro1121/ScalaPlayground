package functional.programming.in.scala.chapter9

import functional.programming.in.scala.chapter7.Par
import functional.programming.in.scala.chapter7.Par.Par

trait Monoid[A] {
  def op(m1: A, m2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    override def op(m1: String, m2: String): String = m1 + m2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(m1: List[A], m2: List[A]): List[A] = m1 ++ m2
    override def zero: List[A] = Nil
  }

  /*
  * 10.1
  * */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(m1: Int, m2: Int): Int = m1 + m2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(m1: Int, m2: Int): Int = m1 * m2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(m1: Boolean, m2: Boolean): Boolean = m1 || m2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(m1: Boolean, m2: Boolean): Boolean = m1 && m2
    override def zero: Boolean = true
  }

  /*
  * 10.2
  * */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(m1: Option[A], m2: Option[A]): Option[A] = m1 orElse m2
    override def zero: Option[A] = None
  }

  /*
  * 10.3
  * endofunction
  * */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(m1: (A) ⇒ A, m2: (A) ⇒ A): (A) ⇒ A = m1 andThen m2
    override def zero: (A) ⇒ A = (a:A) ⇒ a
  }

  /*
  * 10.4
  * */
//  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /*
  * 10.5
  * */
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    concatenate(as.map(f),m)
  }

  /*
  * 10.6
  * */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  /*
  * 10.7
  * */
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val (a,b) = v.splitAt(v.length/2)
    m.op(foldMapV(a,m)(f),foldMapV(b,m)(f))
  }

  /*
  * 10.8
  * */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(m1: Par[A], m2: Par[A]): Par[A] = Par.map2(m1,m2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(Par.asyncF(f)) //different from answer key
  }

  /*
  * 10.9
  * Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
  * You’ll need to come up with a creative Monoid.
  * */
  val isOrderedMonad
  def isOrdered(v: IndexedSeq[Int]): Boolean = {
    ???
  }




}