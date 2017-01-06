package functional.programming.in.Scala.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A] {
  override def toString: String = s"($head, $tail)"
}

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //3.2
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil ⇒ list
      case Cons(_, xs) ⇒ xs
    }

  //3.3
  def setHead[A](a: A, list: List[A]): List[A] =
    list match {
      case Nil ⇒ Cons(a, Nil)
      case Cons(_, xs) ⇒ Cons(a, xs)
    }

  //3.4
  @tailrec
  def drop[A](n: Int, l: List[A]): List[A] =
  n match {
    case x if x <= 0 ⇒ l
    case _ ⇒ drop(n - 1, tail(l))
  }

  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil ⇒ l
      case Cons(x, xs) ⇒ if (f(x)) dropWhile(xs, f) else l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  //3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil ⇒ l
      case Cons(x, Cons(_, Nil)) ⇒ Cons(x, Nil)
      /* this is why this is not efficient
         Constantly having to create a new Cons cell
       */
      case Cons(x, xs) ⇒ Cons(x, init(xs))
    }

  /*
  * This H.O. Function is written with 2 param lists to assist
   * with type inference.
   * It is also curried. So passing only a list will return a
   * HO Function that takes a function as a param
  *
  * Dupe - guess I did it above :-)
  * */
  /*def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }*/

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ns: List[Int]): Int = foldRight(ns, 0)((x,y) => x + y)
  def product(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  //3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) ⇒ acc + 1)

}

object Chapter3 extends App {

  //3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y //winner - i was right !!! bitches !!!
    case Cons(h, t) => h + "some stuff"
    case _ => 101
  }
  println(x)

  val testList = List(1, 2, 3, 4, 5)
  println("List.drop= " + List.drop(2, testList))
  println("List.dropWhile= " + List.dropWhile(testList, (x: Int) ⇒ x < 4))
  println("List.init= " + List.init(testList))
  println("List.length= " + List.length(testList))
}