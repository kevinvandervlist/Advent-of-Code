package nl.kevinvandervlist.aoc2020.day05

import nl.kevinvandervlist.aoc.AoCSpec
import scala.io.Source

class BoardingSpec extends AoCSpec {
  private val input = Source.fromResource("day-05-input").getLines().toList
  override def example: String = ???

  override def examplePartOne(): Any = {
    Seat("FBFBBFFRLR") shouldBe Seat(44, 5)
    Seat("FBFBBFFRLR").seatId shouldBe 357

    Seat("BFFFBBFRRR") shouldBe Seat(70, 7)
    Seat("BFFFBBFRRR").seatId shouldBe 567

    Seat("FFFBBBFRRR") shouldBe Seat(14, 7)
    Seat("FFFBBBFRRR").seatId shouldBe 119

    Seat("BBFFBBFRLL") shouldBe Seat(102, 4)
    Seat("BBFFBBFRLL").seatId shouldBe 820
  }

  override def assignmentPartOne(): Any =
    Boarding.one(input) shouldBe 951

  override def examplePartTwo(): Any =
    succeed

  override def assignmentPartTwo(): Any =
    Boarding.two(input) shouldBe 653
}
