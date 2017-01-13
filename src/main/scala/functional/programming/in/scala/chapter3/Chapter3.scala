package functional.programming.in.scala.chapter3

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

  //3.10
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil ⇒ z
      case Cons(x,xs) ⇒ foldLeft(xs,f(z,x))(f)
    }

  //3.11
  def sumWithFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def productWithFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def lengthWithFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((acc , _) ⇒ acc + 1)

    //3.12
  def reverse[A, B <: A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, elem) ⇒ Cons(elem, acc))

  //3.13
  def foldLeftUsingFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as,z)((elem, acc) ⇒ f(acc, elem))

  def foldRightUsingFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, z)((acc, elem) ⇒ f(elem, acc))

  //3.14
  def appendWithFoldRight[A, B <: A](l1: List[A], l2: List[A]): List[A] =
//    foldLeft(l2, l1)((acc, elem) ⇒ Cons(elem, acc)) //list comes out of order
    foldRight(l1, l2)(Cons(_,_))

  //3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])((acc, elem) ⇒ appendWithFoldRight(acc, elem))

  //3.16
  def addOne(l: List[Int]): List[Int] =
    l match {
      case Nil         => l
      case Cons(x, xs) => Cons(x + 1, addOne(xs))
    }

  //3.17
  def dToS(l: List[Double]): List[String] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons("stringForm="+x.toString, dToS(xs))
    }

  //3.18
  def map[A, B](l: List[A], f: A => B): List[B] =
    l match {
      case Nil         => Nil
      case Cons(x, xs) => Cons(f(x), map(xs, f))
    }

  //3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil         => l
      case Cons(x, xs) => if(f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
    }

  //3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    l match {
      case Nil         => Nil
      case Cons(x, xs) => append(f(x),flatMap(xs)(f))
    }

  //3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)( a => if(f(a)) Cons(a, Nil) else Nil )

  //3.22
  def addEachElement(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (_, Nil) | (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addEachElement(t1, t2))
    }

  //3.23
  def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] =
    (a, b) match {
      case (_, Nil) | (Nil, _)          => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

}

object Chapter3 extends App {
  import List._

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
  val testListToBeAppended = List(6,7,8)
  val testList2 = List(1.0, 2.0, 3.0)
  println("drop= "                   + drop(2, testList))
  println("dropWhile= "              + dropWhile(testList, (x: Int) ⇒ x < 4))
  println("init= "                   + init(testList))
  println("length= "                 + length(testList))
  println("foldLeft= "               + foldLeft(testList, 0)(_ + _))
  println("sumWithFoldLeft= "        + sumWithFoldLeft(testList))
  println("productWithFoldLeft= "    + productWithFoldLeft(testList2))
  println("lengthWithFoldLeft= "     + lengthWithFoldLeft(testList))
  println("reverse= "                + reverse(testList))
  println("appendWithFoldLeft= "     + appendWithFoldRight(testList, testListToBeAppended))
  println("append"                   + append(testList, testListToBeAppended))
  println("foldLeftUsingFoldRight= " + foldLeftUsingFoldRight(testList, 0)(_ + _))
  println("foldRightUsingFoldLeft= " + foldRightUsingFoldLeft(testList, 0)(_ + _))
  println("flatten= "                + flatten(List(testList, testListToBeAppended)))
  println("addOne ="                 + addOne(testList))
  println("dToS ="                   + dToS(testList2))
  println("map(+2)= "                + map(testList, (x:Int) => x + 2))
  println("filter= "                 + filter(testList)((x: Int) => x < 3))
  println("flatMap= "                + flatMap(testList)((x: Int) => Cons(x + 1, Nil)))
  println("filterViaFlatMap= "       + filterViaFlatMap(testList)((x: Int) => x < 4))
  println("addEachElement= "         + addEachElement(testList, testListToBeAppended))
  println("zipWith= "                + zipWith(testList, testListToBeAppended)((a,b) => a + b))
}
