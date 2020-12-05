package nl.kevinvandervlist.aoc2020.day05

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class BoardingSpec extends AnyWordSpec with Matchers {
  private val input = Source.fromResource("day-05-input").getLines().toList

  "Part 1" should {
    "parse first example" in {
      Seat("FBFBBFFRLR") shouldBe Seat(44, 5)
      Seat("FBFBBFFRLR").seatId shouldBe 357
    }
    "parse second example" in {
      Seat("BFFFBBFRRR") shouldBe Seat(70, 7)
      Seat("BFFFBBFRRR").seatId shouldBe 567
    }
    "parse third example" in {
      Seat("FFFBBBFRRR") shouldBe Seat(14, 7)
      Seat("FFFBBBFRRR").seatId shouldBe 119
    }
    "parse fourth example" in {
      Seat("BBFFBBFRLL") shouldBe Seat(102, 4)
      Seat("BBFFBBFRLL").seatId shouldBe 820
    }
    "validate assignment" in {
      Boarding.one(input) shouldBe 951
    }
  }

  "Part 2" should {
    "validate assignment" in {
      Boarding.two(input) shouldBe 653
    }
  }
}
