package functional.programming.in.scala.chapter6

trait RNG {
  import RNG._

  def nextInt: (Int, RNG)
  type Rand[+A] = RNG => (A, RNG)
  type State[S,+A] = S => (A,S)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /*
  * 6.5
  * */
  def double: Rand[Double] = map(nonNegativeInt)(n ⇒ 1/n.toDouble)

  /*
  * 6.6
  * */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng ⇒ {
      val(n, rng2) = ra(rng)
      val(n2, rng3) = rb(rng2)

      (f(n,n2), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /*
  * 6.7
  * */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((rng, b) ⇒ map2(rng, b)(_::_))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    //could replace the second param with with a call to `int`
    val list: List[Rand[Int]] = List.fill(count){ rng ⇒ rng.nextInt }

    sequence(list)(rng)
  }

  /*
  * 6.8
  * */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng ⇒ {
      val (i, rng2) = f(rng)
      g(i)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){
      i ⇒ rng ⇒
        val mod = i % n
        if (i + (n-1) - mod >= 0) (mod, rng)
        else nonNegativeLessThan(n)(rng)
    }
  /*
  * Can also be written as:
  *
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  * */

  /*
  * map and map2
  * */
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a ⇒ unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a,b)))

}

object RNG {

  /*
  * 6.1
  * */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case next@(n, _) if n >= 0 && n <= Int.MaxValue ⇒ next
      case (n, rng2) if n == Int.MinValue || n < 0 ⇒ nonNegativeInt(rng2)
    }
  }

  /*
  * 6.2
  * */
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    (1/n.toDouble, rng2)
  }

  /*
  * 6.3
  * */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n1, rng2) = nonNegativeInt(rng)
    val (d1, rng3) = double(rng2)
    ((n1, d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n,d), rng2) = intDouble(rng)
    ((d, n), rng2)

  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  /*
  * 6.4
  * */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(innerCount: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
      if(innerCount == 0) acc
      else {
        val (xs, r) = acc
        val (x, r2) = nonNegativeInt(r)
        loop(innerCount - 1, (x::xs, r2))
      }
    }

    loop(count, (Nil, rng))
  }

}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, SimpleRNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}



case class State[S,+A](run: S => (A,S))

object State {

  def unit[S, A](a: A): State[S, A] =
    State((state:S) ⇒ (a, state))

}