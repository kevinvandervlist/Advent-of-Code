package nl.kevinvandervlist.aoc2020.day01

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class LedgerSpec extends AnyWordSpec with Matchers {
  private val example =
    """1721
      |979
      |366
      |299
      |675
      |1456
      |""".stripMargin

  private val input = Source.fromResource("day-01-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      Ledger.fromStringsTwo(example.split('\n').toList) shouldBe BigInt(514579)
    }
    "validate assignment" in {
      Ledger.fromStringsTwo(input) shouldBe BigInt(567171)
    }
  }

  "Part 2" should {
    "validate example" in {
      Ledger.fromStringsThree(example.split('\n').toList) shouldBe BigInt(241861950)
    }
    "validate assignment" in {
      Ledger.fromStringsThree(input) shouldBe BigInt(212428694)
    }
  }
}
