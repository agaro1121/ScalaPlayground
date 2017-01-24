package functional.programming.in.scala.chapter6

import org.scalatest.{Matchers, Pending, WordSpec}

class RNGTest extends WordSpec with Matchers {

  "RNG" should {

    "implement nonNegativeInt" in {
      val rng = SimpleRNG(Int.MaxValue)

      val (n, rng2) = RNG.nonNegativeInt(rng)

      n shouldBe 1932566803
      rng2 shouldBe SimpleRNG(126652698007966L)
    }

    "implement double" in {
      val rng = SimpleRNG(Int.MaxValue)
      val (n, rng2) = RNG.double(rng)

      n shouldBe 5.174465371378937E-10
      rng2 shouldBe SimpleRNG(126652698007966L)
    }

    "implement intDouble" in Pending
    "implement doubleInt" in Pending
    "implement double3"   in Pending
    "implement ints"      in Pending

  }

}
