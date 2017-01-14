package functional.programming.in.scala.chapter3

import org.scalatest.{Matchers, Pending, WordSpec}

class TreesTest extends WordSpec with Matchers {

  "Tree" should {

    "size should return correct value" in {
      val testTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

      testTree.size shouldBe 3
    }

    "maximum should return the correct maximum" in {
      val testTree = Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))

      testTree.maximum shouldBe 5
    }

    "maximum depth should be calculated correctly" in {
      val testTree =
        Branch( //1
          Branch( //2
            Leaf(1),
            Branch( //3
              Branch(Leaf(4), Leaf(5)), //4
              Leaf(10)
            )
          ),
          Leaf(3)
        )

      testTree.depth shouldBe 4
    }

    "have a working map function" in {
      val input = Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))
      val output = Branch(Branch(Leaf(2), Leaf(6)), Leaf(4))

      val mapped = input.map(_ + 1)
      mapped shouldBe output
    }

    "have a working fold function" in {
      val input = Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))
//      input.fold(0)(_ + _) shouldBe 9

      val input2 = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
//      input2.fold(0)(_ + _) shouldBe 6

      val input3 =
        Branch( //1
          Branch( //2
            Leaf(1),
            Branch( //3
              Branch(Leaf(4), Leaf(5)), //4
              Leaf(10)
            )
          ),
          Leaf(3)
        )
      Pending
    }

    "sizeWithFold() be the same as size" in {
      val testTree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

      testTree.sizeWithFold shouldBe 3
    }

    "maximumWithFold() be the same as maximum" in {
      val testTree = Branch(Branch(Leaf(1), Leaf(5)), Leaf(3))

      testTree.maximumWithFold shouldBe 5
    }

    "depthWithFold() be the same as depth" in {
      val testTree =
        Branch( //1
          Branch( //2
            Leaf(1),
            Branch( //3
              Branch(Leaf(4), Leaf(5)), //4
              Leaf(10)
            )
          ),
          Leaf(3)
        )
      Pending
    }

    "mapWithFold() be the same as map" in {
      val testTree =
        Branch( //1
          Branch( //2
            Leaf(1),
            Branch( //3
              Branch(Leaf(4), Leaf(5)), //4
              Leaf(10)
            )
          ),
          Leaf(3)
        )
      Pending
    }

  }

}
