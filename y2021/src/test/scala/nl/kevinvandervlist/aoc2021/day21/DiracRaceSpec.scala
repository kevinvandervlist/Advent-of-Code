package nl.kevinvandervlist.aoc2021.day21

import nl.kevinvandervlist.aoc.AoCSpec

class DiracRaceSpec extends AoCSpec {
  override def example: String =
    """Player 1 starting position: 4
      |Player 2 starting position: 8""".stripMargin

  override def examplePartOne(): Any =
    DiracRace.one(exampleAsLines) shouldBe 739785L

  override def assignmentPartOne(): Any =
    DiracRace.one(inputAsLines) shouldBe 604998L

  override def examplePartTwo(): Any =
    DiracRace.two(exampleAsLines) shouldBe 444356092776315L

  override def assignmentPartTwo(): Any =
    DiracRace.two(inputAsLines) shouldBe 157253621231420L
}
