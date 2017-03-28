package functional.programming.in.scala.chapter13

import functional.programming.in.scala.chapter11.Monad

trait IO[A] {
  self =>

  def run: A

  def ++(io: IO[A]): IO[A] = new IO[A] {
    def run = {
      self.run; io.run
    }
  }

  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }

  def map2[B, C](fb: IO[B])(f: (A, B) => C): IO[C] =
    self.flatMap(a => fb.map(b => f(a,b)))

  def **[B, C](fb: IO[B]): IO[(A, B)] = map2(fb)((_,_))

}

object IO extends Monad[IO] {

  override def unit[A](value: A): IO[A] = new IO[A] {
    override def run: A = value
  }

  override def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def empty: IO[Unit] = new IO[Unit] { def run = () }

  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = new IO[Unit] { def run = println(msg) }

  // An IO[Unit] that reads a line from the console and echoes it back
  val echo = ReadLine.flatMap(PrintLine)

  // An IO[Int] that parses an Int by reading a line from the console
  val readInt = ReadLine.map(_.toInt)


  // An IO[(Int,Int)] that parses an (Int,Int) by reading two lines from the console2
  val readInts = readInt ** readInt


}

object Program {
  import IO._

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] = if (p1.score > p2.score) Some(p1)
  else if (p1.score < p2.score) Some(p2)
  else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): IO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))


  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

}

object Main extends App {

  println("Hello")

  Program.converter.run
}