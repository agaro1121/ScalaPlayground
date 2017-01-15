package functional.programming.in.scala.chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case None ⇒ None
      case Some(v) ⇒ Some(f(v))
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map[Option[B]](a ⇒ f(a)).getOrElse(None)
  //map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None ⇒ default
      case Some(v) ⇒ v
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(a ⇒ Some(a)).getOrElse(ob)
  //this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a ⇒ if (f(a)) Some(a) else None)


}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /*
    4.3
  */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
    /*(a, b) match {
      case (_, None) | (None, _) ⇒ None
      case (Some(va), Some(vb)) ⇒ Some(f(va, vb))
    }*/
  a.flatMap(aa ⇒ b.map(bb ⇒ f(aa,bb)))

  /*
   4.4
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    /*a match {
      case Nil          ⇒ Some(Nil)
      case head :: tail ⇒ head.flatMap(headValue ⇒ sequence(tail).map(headValue :: _))
    }*/
  a.foldRight(Some(Nil): Option[List[A]])((elem, acc) ⇒ elem.flatMap(a ⇒ acc.map(listA ⇒ a :: listA)))


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((elem, acc) ⇒ f(elem).flatMap(a ⇒ acc.map(listA ⇒ a :: listA)))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(a ⇒ a)


}