package books.underscoreio.advancedscala


import cats.data.State, State._
import cats.syntax.applicative._

object StateMonad extends App {

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }


  val ar@(state, result) = a.run(10).value //shows both
  val state2 = a.runS(10).value // only shows State
  val result2 = a.runA(10).value // only shows result

  println(ar)
  println(state2)
  println(result2)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val sr@(s, r) = both.run(20).value
  println(sr)

  // ---------------------------------
  // Constructors
  val getDemo = State.get[Int]
  println(getDemo.run(10).value)

  val setDemo = State.set[Int](30)
  println(setDemo.run(10).value)

  val pureDemo = State.pure[Int, String]("Result")
  println(pureDemo.run(10).value)

  val inspectDemo = State.inspect[Int, String](_ + "!")
  println(inspectDemo.run(10).value)

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(10).value)

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val ssrr@(ss, rr) = program.run(1).value
  println(ssrr)
}

object PostFixExample extends App {

  type CalcState[A] = State[List[A], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case a :: b :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }

  println(evalOne("32").run(List.empty).value)

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(pure[List[Int], Int](0) /*0.pure[CalcState]*/) {
      case (acc, string) => acc.flatMap(_ => evalOne(string))
    }
  }

  println(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value)

}