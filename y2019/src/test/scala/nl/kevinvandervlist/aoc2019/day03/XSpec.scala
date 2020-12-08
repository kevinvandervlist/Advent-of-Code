package nl.kevinvandervlist.aoc2020.day03

import nl.kevinvandervlist.aoc2019.day03.X
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class XSpec extends AnyWordSpec with Matchers {
  private val example =
    """
      |""".stripMargin

  private val input = Source.fromResource("day-03-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      X.one(example.split('\n').toList) shouldBe -1
    }
    "validate assignment" in {
      X.one(input) shouldBe -1
    }
  }

  "Part 2" should {
    "validate example" in {
      X.one(example.split('\n').toList) shouldBe -1
    }
    "validate assignment" in {
      X.one(input) shouldBe -1
    }
  }
}
