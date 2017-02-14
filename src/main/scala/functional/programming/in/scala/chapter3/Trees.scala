package functional.programming.in.scala.chapter3

sealed trait Tree[+A] {

/*
 3.25
*/
 def size: Int = {
    def recurse(tree: Tree[A], count: Int = 0): Int = {
      this match {
        case Leaf(_)      => count + 1
        case Branch(l, r) => l.size + r.size
      }
    }

    recurse(this)
  }

/*
 3.26
*/
 def maximum[T >: A](implicit ev0$: T =:= Int): Int = {
   def recurse(tree: Tree[T]): Int = {
     tree match {
       case Leaf(value)  => ev0$(value)
       case Branch(l, r) => l.maximum.max(r.maximum)
     }
   }

   recurse(this)
 }

  /*
   3.27
  */
  def depth: Int = {
    def recurse(tree: Tree[A], count: Int = 0): Int = {
      tree match {
        case Leaf(_)      ⇒ count
        case Branch(l, r) ⇒ recurse(l, count + 1).max(recurse(r, count + 1))
      }
    }

    recurse(this)
  }

  /*
   3.28
  */
  def map[B](f: A ⇒ B): Tree[B] = {
    this match {
      case Leaf(v)      ⇒ Leaf(f(v))
      case Branch(l, r) ⇒ Branch(l.map(f), r.map(f))
    }
  }

  /*
   3.29
   Generalize size, maximum, depth, and map,
   writing a new function fold that abstracts
   over their similarities.
   Reimplement them in terms of this more general function.
  */
  def fold[B](f: A ⇒ B)(g: (B,B) ⇒ B): B = {
    this match {
      case Leaf(v)      ⇒ f(v)
      case Branch(l, r) ⇒ g(l.fold(f)(g), r.fold(f)(g))
    }
  }

  def sizeWithFold: Int =
    fold(a ⇒ 1)(1 + _ + _)

  def maximumWithFold[T >: A](implicit ev$0: T =:= Int): Int =
    fold(a ⇒ ev$0(a))(_ max _)

  def depthWithFold: Int =
  //Left -> case Leaf(_) ⇒ count
    fold(a ⇒ 0)((d1, d2) ⇒ 1 + (d1 max d2))

  def mapWithFold[B](f: A ⇒ B): Tree[B] =
    fold(a ⇒ Leaf(f(a)): Tree[B])(Branch(_,_))

}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]