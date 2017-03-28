package functional.programming.in.scala.chapter13

import functional.programming.in.scala.chapter11.Monad

trait BadIO[A] {
  self =>

  def run: A

  def ++(io: BadIO[A]): BadIO[A] = new BadIO[A] {
    def run = {
      self.run; io.run
    }
  }

  def map[B](f: A => B): BadIO[B] =
    new BadIO[B] { def run = f(self.run) }

  def flatMap[B](f: A => BadIO[B]): BadIO[B] =
    new BadIO[B] { def run = f(self.run).run }

  def map2[B, C](fb: BadIO[B])(f: (A, B) => C): BadIO[C] =
    self.flatMap(a => fb.map(b => f(a,b)))

  def **[B, C](fb: BadIO[B]): BadIO[(A, B)] = map2(fb)((_,_))

}

object BadIO extends Monad[BadIO] {

  override def unit[A](value: A): BadIO[A] = new BadIO[A] {
    override def run: A = value
  }

  override def flatMap[A, B](ma: BadIO[A])(f: A => BadIO[B]): BadIO[B] = ma flatMap f

  def apply[A](a: => A): BadIO[A] = unit(a)

  def empty: BadIO[Unit] = new BadIO[Unit] { def run = () }

  def ReadLine: BadIO[String] = BadIO { readLine }
  def PrintLine(msg: String): BadIO[Unit] = new BadIO[Unit] { def run = println(msg) }

  // An IO[Unit] that reads a line from the console and echoes it back
  val echo = ReadLine.flatMap(PrintLine)

  // An IO[Int] that parses an Int by reading a line from the console
  val readInt = ReadLine.map(_.toInt)


  // An IO[(Int,Int)] that parses an (Int,Int) by reading two lines from the console2
  val readInts = readInt ** readInt


}

object Program {
  import BadIO._

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] = if (p1.score > p2.score) Some(p1)
  else if (p1.score < p2.score) Some(p2)
  else None

  def winnerMsg(p: Option[Player]): String = p map {
    case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def contest(p1: Player, p2: Player): BadIO[Unit] =
    PrintLine(winnerMsg(winner(p1, p2)))


  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter: BadIO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

}

object Main extends App {

  println("Hello")

  Program.converter.run
}