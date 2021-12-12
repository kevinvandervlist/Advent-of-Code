package nl.kevinvandervlist.aoc2021.day12

import nl.kevinvandervlist.aoc.AoCSpec

class PassagePathingSpec extends AoCSpec {
  override def example: String =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin

  override def examplePartOne(): Any =
    PassagePathing.one(exampleAsLines) shouldBe 10

  override def assignmentPartOne(): Any =
    PassagePathing.one(inputAsLines) shouldBe 4241

  override def examplePartTwo(): Any =
    PassagePathing.two(exampleAsLines) shouldBe 36

  override def assignmentPartTwo(): Any =
    PassagePathing.two(inputAsLines) shouldBe 122134
}
