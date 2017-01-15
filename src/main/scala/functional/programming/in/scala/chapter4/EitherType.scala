package functional.programming.in.scala.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case e@Left(_) ⇒ e
      case Right(v)  ⇒ Right(f(v))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case e@Left(_) ⇒ e
      case Right(v)  ⇒ f(v)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)    ⇒ b
      case r@Right(_) ⇒ r
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(t ⇒ b.map(bb ⇒ f(t, bb)))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    /*es match {
      case Nil    ⇒ Right(Nil)
      case h :: t ⇒ h.flatMap(hh ⇒ sequence(t).map(hh :: _))
    }*/
  es.foldRight(Right(Nil): Either[E, List[A]])((elem, acc) ⇒ elem.flatMap(hh ⇒ acc.map(hh :: _)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    /*as match {
      case Nil    ⇒ Right(Nil)
      case h :: t ⇒ f(h).flatMap(hh ⇒ traverse(t)(f).map(hh :: _))
    }*/
  as.foldRight(Right(Nil): Either[E, List[B]])((elem, acc) ⇒ f(elem).flatMap(hh ⇒ acc.map(hh :: _)))

  def sequenceViaTraversal[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(t ⇒ t)

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] = try Right(a)
  catch { case e: Exception => Left(e) }


}