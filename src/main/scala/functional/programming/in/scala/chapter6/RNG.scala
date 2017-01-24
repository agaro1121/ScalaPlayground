package functional.programming.in.scala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case next@(n, _) if n >= 0 && n <= Int.MaxValue ⇒ next
      case (n, rng2) if n == Int.MinValue || n < 0 ⇒ nonNegativeInt(rng2)
    }
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
