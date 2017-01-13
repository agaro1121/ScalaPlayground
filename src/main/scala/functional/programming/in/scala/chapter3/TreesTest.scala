package functional.programming.in.scala.chapter3

import org.scalatest.{Matchers, WordSpec}

class TreesTest extends WordSpec with Matchers {

  "Tree" should {

    "size should return correct value" in {
      val testTree =  Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

      testTree.size shouldBe 3
    }

    "maximum should return the correct maximum" in {
      val testTree =  Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))

      testTree.maximum shouldBe 5
    }

    "maximum depth should be calculated correctly" in {
      val testTree =
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Branch(Leaf(4), Leaf(5)),
              Leaf(10)
            )
          ),
          Leaf(3)
        )

      testTree.depth shouldBe 4
    }
  }

}
