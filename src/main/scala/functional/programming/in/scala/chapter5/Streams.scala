package functional.programming.in.scala.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }

  /*
  * 5.1
  * */
  def toList: List[A] =
    this match {
      case Empty ⇒ Nil
      case Cons(h, t) ⇒ h() :: t().toList
    }

  /*
  * 5.2
  * */
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 ⇒ cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 ⇒ cons(h(), empty) //handle n==1 to avoid looking at the tail
      case _ ⇒ empty
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n == 0 ⇒ t().drop(n - 1)
      case Empty ⇒ this
    }

  /*
  * 5.3
  * */
  def takeWhile(f: A ⇒ Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) ⇒ cons(h(), t().takeWhile(f))
      case _ ⇒ empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) ⇒ f(h(), t().foldRight(z)(f))
      case _ ⇒ z
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) //b never gets evaluated if p(a) is true. Ergo, early termination

  /*
  * 5.4
  * */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  /*
  * 5.5
  * */
  def takeWhileViaFoldRight(f: A ⇒ Boolean): Stream[A] =
    foldRight(empty: Stream[A])((elem, acc) ⇒ if(f(elem)) cons(elem, acc.takeWhileViaFoldRight(f)) else empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

