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

  }

}
