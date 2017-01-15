package functional.programming.in.scala.chapter4

import org.scalatest.{Matchers, WordSpec}

class OptionsTest extends WordSpec with Matchers {

  "Options" should {

    "implement sequence() correctly" in {
      val input = List(Some(1), Some(2), Some(3))

      Option.sequence(input) shouldBe Some(List(1,2,3))
    }

    "implement sequence() with Nones correctly" in {
      val input = List(Some(1), Some(2), None, Some(3))

      Option.sequence(input) shouldBe None
    }

    "implement traverse() correctly" in {
      val input = List(1,2,3)

      Option.traverse[Int, Int](input)(a ⇒ Some(a + 1)) shouldBe Some(List(2,3,4))
    }

    "implement traverse() with Nones correctly" in {
      val input = List(1,2,3)

      def Try[A](a: => A): Option[A] =
        try Some(a)
        catch { case _:Exception => None }

      Option.traverse[Int, Int](input)(a ⇒ Try(a / 0)) shouldBe None
    }

    "implement sequenceViaTraverse() correctly" in {
      val input = List(Some(1), Some(2), Some(3))

      Option.sequenceViaTraverse(input) shouldBe Some(List(1,2,3))
    }

    "implement sequenceViaTraverse() with Nones correctly" in {
      val input = List(Some(1), Some(2), None, Some(3))

      Option.sequenceViaTraverse(input) shouldBe None
    }

  }

}
