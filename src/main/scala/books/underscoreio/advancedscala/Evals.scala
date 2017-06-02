package books.underscoreio.advancedscala

import cats.Eval

object Evals extends App {


  val now = Eval.now(1 + 2)

  val later = Eval.later(3 + 4)

  val always = Eval.always(5 + 6)

  println(now.value)
  println(later.value)
  println(always.value)

  val x = Eval.now {
    println("Computing X")
    1+1
  }

  println(x.value) //memoized -> like `val`
  println(x.value) // will show "Computing X" upon instantiation

  // ---------------------------------
  val y = Eval.always {
    println("Computing Y")
    1+1
  }

  println(y.value) // NOT memoized -> like `def`
  println(y.value) // will always show "Computing X"


  // ---------------------------------
  val z = Eval.later {
    println("Computing Z")
    1+1
  }

  println(z.value) // memoized -> like `lazy val`
  println(z.value) // will only show "Computing Z" once when called upon

}

object EvalMonad extends App {

  /*
  * map and flatMap
  * are lazily evaluated
  * (on demand)
  * */
  val greeting = Eval.always {
    println("Step 1")
    "Hello"
  }.map { str =>
    println("Step 2")
    str + " world"
  }

  println(greeting.value)

  val ans = for {
    a <- Eval.now    { println("Calculating A") ; 40 } //shows "Calculating A" without calling `ans.value`
    b <- Eval.always { println("Calculating B") ; 2  }
  } yield {
    println("Adding A and B")
    a+b
  }

  println(ans.value)
  println("**"+ans.value)


  // ---------------------------------
  val saying = Eval.always {
    println("Step 1")
    "The cat"
  }.map { str =>
    println("Step 2")
    s"$str sat on"
  }.memoize.map { str => // anything before `.memoize` gets cached
    println("Step 3")
    s"$str the mat"
  }

  println(saying.value)
  println(saying.value) // "Step 1" "Step 2" don't show up here. That all gets memoized

  def factorial(n: BigInt): Eval[BigInt] =
    if(n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(50000).value)

}