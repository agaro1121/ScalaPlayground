package scala.in.depth.types.hlist

/**
  * Created by Hierro on 5/9/16.
  */
trait HList {}


final case class HCons[H, T <: HList](head: H, tail: T) extends HList {
  def ::[T](v: T) = HCons(v, this)

  /*@annotation.tailrec
  def :::[T](v: T):T = v match {
    case x : HNil => this
    case x : HCons => (this.::(x.head)).:::(x.tail)
  }*/

  override def toString = head + " :: " + tail
}

final class HNil extends HList {
  def ::[T](v: T) = HCons(v, this)
  def :::[T](v: T) = v

  override def toString = "Nil"
}

object HList {
  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons
  val HNil = new HNil
}

sealed trait IndexedView {
  type Before <: HList
  type After <: HList
  type At
  def fold[R](f : (Before, At, After) => R) : R
  def get = fold( (_, value, _) => value)
}

/*class HListView[H, T <: HList](val list : H :: T)
  extends IndexedView {
  type Before = HNil
  type After = T
  type At = H

  import HList._

  def fold[R](f : (Before, At, After) => R): R =
    f(HNil, list.head, list.tail)

/*  def remove = fold {
    (before, _, after) => before ::: after
  }
  def insertBefore[B](x : B) = fold {
    (before, current, after) =>
      before ::: (x :: current :: after)
  }
  def replace[B](x : B) = fold {
    (before, _, after) => before ::: (x :: after)
  }
  def insertAfter[B](x : B) = fold {
    (before, current, after) => before ::: (current :: x :: after)
  }*/
}*/




object Main extends App {
  import HList._
  val x : ( String :: Int :: Boolean :: HNil) = "Hi" :: 5 :: false :: HNil

  val one :: two :: three :: HNil = x
  println(one :: two :: three :: HNil)
  /*
  scala> val one :: two :: three :: HNil = x
         one: String = Hi
         two: Int = 5
         three: Boolean = false
   */

  var first :: second :: rest = x
  println(first :: second :: rest)
  /*
  scala> val first :: second :: rest = x
         first: String = Hi
         second: Int = 5
         rest: HList.::[Boolean,HNil] = false :: Nil
   */

  def indexAt2of3[A,B,C]( b : (A :: B :: C :: HNil)) =
    b match {
         case a :: b :: c :: HNil => b
    }

  val a = indexAt2of3( 1 :: false :: "Hi" :: HNil )
  println(a)
}

