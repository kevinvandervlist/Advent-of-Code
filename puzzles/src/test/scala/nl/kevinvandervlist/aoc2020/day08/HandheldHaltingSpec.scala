package nl.kevinvandervlist.aoc2020.day08

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class HandheldHaltingSpec extends AnyWordSpec with Matchers {
  private val example =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6
      |""".stripMargin

  private val input = Source.fromResource("day-08-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      HandheldHalting.one(example.split('\n').toList) shouldBe 5
    }
    "validate assignment" in {
      HandheldHalting.one(input) shouldBe 1723
    }
  }

  "Part 2" should {
    "validate example" in {
      HandheldHalting.two(example.split('\n').toList) shouldBe 8
    }
    "validate assignment" in {
      HandheldHalting.two(input) shouldBe 846
    }
  }
}
