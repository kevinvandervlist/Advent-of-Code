package nl.kevinvandervlist.aoc2020.day06

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class CustomsDeclarationSpec extends AnyWordSpec with Matchers {
  private val example =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b
      |""".stripMargin

  private val input = Source.fromResource("day-06-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      CustomsDeclaration.one(example) shouldBe 11
    }
    "validate assignment" in {
      CustomsDeclaration.one(input.mkString("\n")) shouldBe 6768
    }
  }

  "Part 2" should {
    "validate example" in {
      CustomsDeclaration.two(example) shouldBe 6
    }
    "validate assignment" in {
      CustomsDeclaration.two(input.mkString("\n")) shouldBe 3489
    }
  }
}
