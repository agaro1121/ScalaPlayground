package functional.programming.in.scala.chapter10

import functional.programming.in.scala.chapter3.{Branch, Leaf, Tree}
import functional.programming.in.scala.chapter7.Par
import functional.programming.in.scala.chapter7.Par.Par

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  /*
  * 10.15
  * */
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(Nil: List[A])(_ :: _)
}

object Foldable {
  /*
  * 10.12
  * Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
  * */
  implicit val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }

  implicit val foldableIndexedSeq = new Foldable[IndexedSeq] {
    import Monoid._
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
  }

  implicit val foldableStream = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }

  /*
  * 10.13
  * */
  implicit val foldableTree = new Foldable[Tree] {

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as.fold(f)(mb.op)
  }

  /*
  * 10.14
  *
  * Write a Foldable[Option] instance.
  * */
  implicit val foldableOption = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }


}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
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
  * */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A) = a1 andThen a2
    override def zero = (a: A) => a
  }

  /*
  * 10.4
  * */
  //  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /*
  * 10.5
  * */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    concatenate(as.map(f), m)
  }

  /*
  * 10.6
  * */
  def foldRight[A, B](z: B)(f: (A, B) => B)(as: List[A]): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](z: B)(f: (B, A) => B)(as: List[A]): B = {
    foldMap(as, endoMonoid[B])((a: A) => f(_, a))(z)
  }

  /*
  * 10.7
  * */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val (left, right) = v.splitAt(v.length / 2)
    m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

  /*
  * 10.8
  * */
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(Par.asyncF(f))
  }

  /*
  * 10.10
  * */
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(chars), Stub(chars2)) => Stub(chars ++ chars2)
      case (Stub(chars), Part(lstub, words, rStub)) => Part(chars ++ lstub, words, rStub)
      case (Part(lstub, words, rStub), Stub(chars)) => Part(lstub, words, rStub + chars)
      case (Part(lstub1, words1, r1), Part(l2, words2, rStub2)) =>
        Part(lstub1, words1 + words2 + (if ((r1 + l2).isEmpty) 0 else 1), rStub2)
    }
    override def zero = Stub("")
  }

  /*
  * 10.11
  * */
  def count(s: String): Int = {
    def wc(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }

    def unstub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(chars) => unstub(chars)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

}

