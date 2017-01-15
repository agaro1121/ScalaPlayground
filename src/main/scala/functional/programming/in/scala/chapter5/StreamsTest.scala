package functional.programming.in.scala.chapter5

import org.scalatest.{Matchers, WordSpec}

class StreamsTest extends WordSpec with Matchers {

  "Streams" should {

    "implement headOption() correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      input.headOption shouldBe Some(1)
    }

    "implement take(n) correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      input.take(3).toList shouldBe List(1, 2, 3)
    }

    "implement drop(n) correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      input.drop(3).toList shouldBe List(4, 5)
    }

    "implement takeWhile(p) correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      val output = input.takeWhile(_ < 4).toList
      output shouldBe List(1, 2, 3)
    }

    "implement forAll(p) correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      val output = input.forAll(_ == 3)
      output shouldBe false
    }

    "implement takeWhileViaFoldRight(p) correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      val output = input.takeWhileViaFoldRight(_ < 4).toList
      output shouldBe List(1, 2, 3)
    }

  }

}
