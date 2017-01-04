package functional.programming.in.Scala.chapter2

/**
 * Created by hierro on 9/27/15.
 */
object Chapter2 {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def fibonacci(n: Int): Int = n match {
    case 0 | 1 => n
    case _ if n > 1 => fibonacci(n - 1) + fibonacci(n - 2)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    var bool: Boolean = false
    var x = 0;
    do {
      bool = gt(as(x), as(x + 1))
      x+=1
    } while (bool != false && x < as.length - 2)
    bool
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    //   f(a, _: B)
    //  _ ⇒ f(a)
    //    (b: B) ⇒ f(a, b)
    f(a, _)
  }

  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) ⇒ (b: B) ⇒ f(a,b)

  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a:A, b:B) ⇒ f(a)(b)

  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) ⇒ f(g(a))





}
