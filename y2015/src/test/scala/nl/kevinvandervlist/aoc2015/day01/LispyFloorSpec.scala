package nl.kevinvandervlist.aoc2015.day01

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class LispyFloorSpec extends AnyWordSpec with Matchers {
  private val input = Source.fromResource("day-01-input").getLines().toList.head

  "Part 1" should {
    "validate example" in {
      val example =
        """(())
          |()()
          |(((
          |(()(()(
          |))(((((
          |())
          |))(
          |)))
          |)())())
          |""".stripMargin
      val splitted = example.split('\n')
      LispyFloor.one(splitted(0)) shouldBe 0
      LispyFloor.one(splitted(1)) shouldBe 0
      LispyFloor.one(splitted(2)) shouldBe 3
      LispyFloor.one(splitted(3)) shouldBe 3
      LispyFloor.one(splitted(4)) shouldBe 3
      LispyFloor.one(splitted(5)) shouldBe -1
      LispyFloor.one(splitted(6)) shouldBe -1
      LispyFloor.one(splitted(7)) shouldBe -3
      LispyFloor.one(splitted(8)) shouldBe -3
    }
    "validate assignment" in {
      LispyFloor.one(input) shouldBe 74
    }
  }

  "Part 2" should {
    "validate example" in {
      val example =
        s""")
           |()())
           |""".stripMargin
      val splitted = example.split('\n')
      LispyFloor.two(splitted(0)) shouldBe 1
      LispyFloor.two(splitted(1)) shouldBe 5
    }
    "validate assignment" in {
      LispyFloor.two(input) shouldBe 1795
    }
  }
}
