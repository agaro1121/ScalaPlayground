package functional.programming.in.scala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
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
