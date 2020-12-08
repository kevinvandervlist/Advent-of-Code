package nl.kevinvandervlist.aoc2020.day02

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class PasswordPolicySpec extends AnyWordSpec with Matchers {
  private val example =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin

  private val input = Source.fromResource("day-02-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      PasswordPolicy.validPasswordsOne(example.split('\n').toList) shouldBe 2
    }
    "validate assignment" in {
      PasswordPolicy.validPasswordsOne(input) should not be 477
      PasswordPolicy.validPasswordsOne(input) shouldBe 603
    }
  }

  "Part 2" should {
    "validate example" in {
      PasswordPolicy.validPasswordsTwo(example.split('\n').toList) shouldBe 1
    }
    "validate assignment" in {
      PasswordPolicy.validPasswordsTwo(input) shouldBe 404
    }
  }
}
