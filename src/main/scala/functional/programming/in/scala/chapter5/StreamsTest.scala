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

    "implement headOptionViaFoldRight() correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      input.headOptionViaFoldRight shouldBe Some(1)
    }

    "implement mapViaFoldRight() correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      val output = input.map(_ + 1).toList
      output shouldBe List(2, 3, 4, 5, 6)
    }

    "implement filterviaFoldRight correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      input.filter(_ < 4).toList shouldBe List(1, 2, 3)
      input.filter(_ > 2).toList shouldBe List(3, 4, 5)
    }

    "implement appendViaFoldRight correctly" in {
      val input = Stream(1, 2, 3, 4, 5)
      val input2 = Stream(6, 7, 8)

      input.append(input2).toList shouldBe List(1, 2, 3, 4, 5, 6, 7, 8)
    }

    "implement flatMapViaFoldRight() correctly" in {
      val input = Stream(1, 2, 3, 4, 5)

      val output = input.flatMap(i ⇒ Stream(i + 1)).toList
      output shouldBe List(2, 3, 4, 5, 6)
    }

    "implement constant() correctly" in {
      val input = Stream.constant(1)

      input.take(1).toList shouldBe List(1)
      input.take(2).toList shouldBe List(1, 1)
      input.take(3).toList shouldBe List(1, 1, 1)
      input.take(4).toList shouldBe List(1, 1, 1, 1)
      input.take(10).toList shouldBe List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    }

    "implement from() correctly" in {
      val input = Stream.from(5)

      input.take(1).toList shouldBe List(5)
      input.take(2).toList shouldBe List(5, 6)
      input.take(3).toList shouldBe List(5, 6, 7)
      input.take(4).toList shouldBe List(5, 6, 7, 8)
      input.take(10).toList shouldBe List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    }

    "implement fibs correctly" in {
      val input = Stream.fibs

      input.take(3).toList shouldBe List(0, 1, 1)
      input.take(4).toList shouldBe List(0, 1, 1, 2)
      input.take(5).toList shouldBe List(0, 1, 1, 2, 3)
    }

    "implement unfold correctly" in {
      val input = Stream.unfold(0)(s ⇒ Some(s, s + 1))

      input.take(3).toList shouldBe List(0, 1, 2)
      input.take(4).toList shouldBe List(0, 1, 2, 3)
      input.take(5).toList shouldBe List(0, 1, 2, 3, 4)
    }

    "implement fibsViaUnfold() correctly" in {
      val input = Stream.fibsViaUnfold

      input.take(3).toList shouldBe List(0, 1, 1)
      input.take(4).toList shouldBe List(0, 1, 1, 2)
      input.take(5).toList shouldBe List(0, 1, 1, 2, 3)
    }



  }

}
