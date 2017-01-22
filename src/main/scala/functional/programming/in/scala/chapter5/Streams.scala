package functional.programming.in.scala.chapter5

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
    foldRight(empty: Stream[A])((elem, acc) ⇒ if (f(elem)) cons(elem, acc.takeWhileViaFoldRight(f)) else empty)

  /*
  * 5.6
  * */
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) ⇒ Some(h))

  /*
  * 5.7
  * */
  def map[B](f: ⇒ A ⇒ B): Stream[B] =
    foldRight(empty: Stream[B])((elem, acc) ⇒ cons(f(elem), acc))

  def filter(p: ⇒ A ⇒ Boolean): Stream[A] =
    foldRight(empty: Stream[A])(
      (elem, acc) ⇒
        if (p(elem))
          cons(elem, acc)
        else
          acc
    )

  def append[B >: A](s: ⇒ Stream[B]): Stream[B] =
    foldRight(s)((elem, acc) ⇒ cons(elem, acc))

  def flatMap[B](f: ⇒ A ⇒ Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((elem, acc) ⇒ f(elem).append(acc))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  /*
  * 5.13
  * map, take, takeWhile, zipWith, zipAll(in trait)
  * */
  def mapViaUnfold[B](f: ⇒A ⇒ B): Stream[B] =
    unfold(this){
      case Cons(h, t) ⇒ Some( (f(h()), t()) )
      case _ ⇒ None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this){
      case Cons(h, t) if n > 1 ⇒ Some(h(), t().takeViaUnfold(n - 1))
      case Cons(h, _) if n == 1 ⇒ Some(h(), empty) //handle n==1 to avoid looking at the tail
      case _ ⇒ None
    }

  def takeWhileViaUnfold(f: A ⇒ Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if f(h()) ⇒ Some(h(), t().takeWhileViaUnfold(f))
      case _ ⇒ None
    }

  def zipWith[B >: A](b: Stream[B])(f: (B,B) => B): Stream[B] = ???

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???

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

  /*
   * 5.8
   * */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /*
  * 5.9
  * */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /*
  * 5.10
  * */
  def fibs: Stream[Int] = {

    def inner(n: Int, nextN: Int): Stream[Int] =
      cons(n, inner(nextN, n + nextN))

    inner(0, 1)
  }

  /*
  * 5.11
  * */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
  * 5.12
  * Write fibs, from, constant, and ones in terms of unfold
  * */
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (n, nextN) ⇒ Some((n, (nextN, nextN + n)))
    }

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s ⇒ Some(s, s + 1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s ⇒ Some(s, s))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val onesViaUnfold: Stream[Int] = unfold(1)(s ⇒ Some(s,s))






}

