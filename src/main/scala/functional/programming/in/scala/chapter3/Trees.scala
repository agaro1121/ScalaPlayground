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


}


case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

