package functional.programming.in.scala.chapter13

import functional.programming.in.scala.chapter11.Monad
import functional.programming.in.scala.chapter13.Translate.~>
import functional.programming.in.scala.chapter7.Par
import functional.programming.in.scala.chapter7.Par.Par

trait Free[F[_], A] {

  /*
  * 13.1
  * */
  def map[B](f: A => B): Free[F, B] = flatMap(fa => Return(f(fa)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

}
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class Return[F[_], A](value: A) extends Free[F, A]
case class FlatMap[F[_], A, B](free: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  /*
  * 13.1
  * */
  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] =
    new Monad[({ type f[a] = Free[F, a] })#f] {
      override def flatMap[A, B](ma: Free[F, A])(f: (A) => Free[F, B]): Free[F, B] = ma flatMap f
      override def unit[A](value: A): Free[F, A] = Return(value)
    }

  /*
  * 13.2
  * */
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return(a) => a
    case Suspend(resume) => resume()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(resume) => runTrampoline(f(resume()))
      case FlatMap(y, g) => runTrampoline(y.flatMap(a => g(a).flatMap(f)))
    }
  }

  /*
  * 13.3
  * */
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = {
    a match {
      case Return(value) => F.unit(value)
      case Suspend(resume) => resume
      case FlatMap(Suspend(x), f) => F.flatMap(x)(a => run(f(a)))
    }
  }

  @annotation.tailrec
  def step[F[_], A](free: Free[F, A]): Free[F, A] = free match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => free
  }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

}

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = ???
  //    Par.lazyUnit(Par.run)

  override def toThunk: () => Option[String] =
    try {
      () => Some(readLine())
    } catch {
      case _: Exception => () => None
    }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val f1: Free[Console, Option[String]] = for {
    _ <- printLn("I can only interact with the console.")
    ln <- readLn
  } yield ln

  import Translate.~>
  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar =
    new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }

}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
object Translate {
  type ~>[F[_], G[_]] = Translate[F, G]

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G, A] = Suspend { fg(a) }
    }
    Free.runFree(f)(t)(Free.freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A =
    Free.runTrampoline {
      translate(a)(new (Console ~> Function0) {
        def apply[A](c: Console[A]) = c.toThunk
      })
    }

}