package nl.kevinvandervlist.aoc2016.day01

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class TaxiCabSpec extends AnyWordSpec with Matchers {
  private val example =
    """R2, L3
      |R2, R2, R2
      |R5, L5, R5, R3
      |""".stripMargin

  private val input = Source.fromResource("day-01-input").getLines().toList.head

  "Part 1" should {
    "validate example" in {
      val splitted = example.split('\n').toList
      TaxiCab.one(splitted(0)) shouldBe 5
      TaxiCab.one(splitted(1)) shouldBe 2
      TaxiCab.one(splitted(2)) shouldBe 12
    }
    "validate assignment" in {
      TaxiCab.one(input) shouldBe 299
    }
  }

  "Part 2" should {
    "validate example" in {
      TaxiCab.two("R8, R4, R4, R8") shouldBe 4
    }
    "validate assignment" in {
      TaxiCab.two(input) shouldBe 181
    }
  }
}
