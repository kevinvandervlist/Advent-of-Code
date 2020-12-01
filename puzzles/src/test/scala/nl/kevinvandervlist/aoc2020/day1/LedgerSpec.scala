package nl.kevinvandervlist.aoc2020.day1

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

  private val input = Source.fromResource("day-1-input").getLines().toList

  "A small ledger: 2" should {
    "compute the right totals" in {
      Ledger.fromStringsTwo(example.split('\n').toList) shouldBe BigInt(514579)
    }
  }
  "A large ledger: 2" should {
    "compute the result" in {
      Ledger.fromStringsTwo(input) shouldBe BigInt(567171)
    }
  }

  "A small ledger: 3" should {
    "compute the right totals" in {
      Ledger.fromStringsThree(example.split('\n').toList) shouldBe BigInt(241861950)
    }
  }
  "A large ledger: 3" should {
    "compute the result" in {
      val input = Source.fromResource("day-1-input").getLines().toList
      Ledger.fromStringsThree(input) shouldBe BigInt(212428694)
    }
  }
}
