package functional.programming.in.scala.chapter8

import functional.programming.in.scala.chapter6.{RNG, SimpleRNG, State}

trait Prop {
  import Prop._

  def &&(p: Prop): Prop = new Prop {
    override def check = (Prop.this.check, p.check) match {
      case (Left((f1, s1)), Left((f2, s2))) ⇒ Left((s"$f1\n$f2", s1 + s2))
      case (Left((f1, s1)), Right(s2)) ⇒ Left((f1, s2 + s1))
      case (Right(s1), Left((f2, s2))) ⇒ Left((f2, s2 + s1))
      case (Right(s1), Right(s2)) ⇒ Right(s1 + s2)
    }

  }

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}

trait Gen[A] {

  def listOf[A](a: Gen[A]): Gen[List[A]]
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop

}

object Gen {

  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a ⇒ f(a).sample))
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n ⇒ Gen(State.sequence(List.fill(n)(sample))))
    }

  }

  /*
  * 8.4
  * */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val state = State(RNG.nonNegativeInt)
      .map(n => start + n % (stopExclusive - start))

    Gen(state)
  }

  /*
  * 8.5
  * */
  def unit[A](a: => A): Gen[A] = {
    val state = State.unit[RNG, A](a)

    Gen(state)
  }

  def boolean: Gen[Boolean] = {
    val state = State(RNG.nonNegativeInt)
      .map(n => if (n % 2 == 0) true else false)

    Gen(state)
  }

  //list has all dupes. Doh!
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

}