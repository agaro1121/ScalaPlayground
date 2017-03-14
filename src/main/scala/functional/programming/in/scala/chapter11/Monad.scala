package functional.programming.in.scala.chapter11

import functional.programming.in.scala.chapter11.State.IntState
import functional.programming.in.scala.chapter6.{RNG, SimpleRNG}

import scala.language.{higherKinds, reflectiveCalls}

trait Monad[M[_]] {
  def unit[A](value: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  /*
  * 11.3
  *
  * Could also be implemented with Folds
  * */
  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma match {
      case Nil => unit(Nil)
      case x :: xs => map2(x, sequence(xs))(_ :: _)
    }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la match {
      case Nil => unit(Nil)
      case xs => sequence(xs.map(f))
    }

  /*
  * 11.4
  * */
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    //    sequence(List.fill(n)(ma))
    //  traverse(List.fill(n)(ma))(x => x)
    n match {
      case 0 => unit(Nil)
      case x => map2(ma, replicateM(x - 1, ma))(_ :: _)
    }

  /*
  * 11.6
  * */
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(Nil: List[A])) {
      (a, lmb) =>
        map2(f(a), lmb)((b, as) => if (b) a :: as else as)
    }

  /*
  * 11.7
  * */
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => flatMap(f(a))(g)

  /*
  * 11.8
  * */
  def flatMapViaCompose[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit, A, B]((_: Unit) => ma, f)()

  /*
  * 11.12
  * */
  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(ma => ma)

  /*
  * 11.13
  * */
  def flatMapViaJoin[A, B](ma: M[A])(f: A => M[B]): M[B] = {
    join(map(ma)(f))
  }

  def composeViaJoin[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (a: A) => join(map(f(a))(g))

}

object Monad {

  /*
  * 11.1
  * */
  val optionMonad = new Monad[Option] {
    override def unit[A](value: A): Option[A] = Some(value)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](value: A): Stream[A] = Stream(value)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](value: A): List[A] = List(value)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }

  /*
  * 11.17
  * */
  val idMonad = new Monad[Id] {
    override def unit[A](value: A): Id[A] = Id(value)
    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = ma flatMap f
  }

  val intStateMonad = new Monad[IntState] {
    def unit[A](a: A): IntState[A] = State(s => (a, s))
    def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] = st flatMap f
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

}

/*
* 11.17
* */
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

/*
* 11.20
* */
case class Reader[R, A](run: R => A)
object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: A): Reader[R, A] = Reader(r => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = join(map(st)(f))
  }
}

object Main extends App {
  import Monad._

  println(listMonad.compose[Int, Int, String](a => List(a + 1), b => List("String=" + b.toString))(5))

  println(listMonad.compose[Int, Int, Int](a => List(a), listMonad.unit)(5))
  println(listMonad.compose[Int, Int, Int](listMonad.unit, a => List(a))(5))

  import functional.programming.in.scala.chapter6.RNG._

  private val negativeInt: (RNG) => (Int, RNG) = nonNegativeInt
  private val state: State[RNG, Int] = State[RNG, Int](negativeInt)

  println(stateMonad.replicateM(2, state).run(SimpleRNG(3L)))
  println("sequence = " + stateMonad.sequence(List.fill(2)(state)).run(SimpleRNG(3L)))
  println(stateMonad.sequence(List.fill(2)(state)).run(SimpleRNG(3L)))

}

