package functional.programming.in.Scala

import java.time.LocalDate

import functional.programming.in.Scala.chapter2.Chapter2
import functional.programming.in.Scala.chapter2.Chapter2._

/**
 * Created by hierro on 9/27/15.
 */
object Main {
  def main(args: Array[String]) {
    println("Running Main...")
    println(s"Fib 6 = ${Chapter2.fibonacci(6)}")
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => {
      val r = x + 1; r
    }))

    val l = Array[Int](1, 2, 3, 2, 5, 4)
    println(s"is Sorted ? ${isSorted(l, (x: Int, y: Int) => x < y)}")
  }


}

sealed abstract case class Conf(name: String)
object Conf {
  def apply(name: String): Conf = new Conf(name + " is taking over the world"){}
}
