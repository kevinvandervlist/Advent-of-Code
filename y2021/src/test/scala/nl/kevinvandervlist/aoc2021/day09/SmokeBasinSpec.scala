package nl.kevinvandervlist.aoc2021.day09

import nl.kevinvandervlist.aoc.AoCSpec

class SmokeBasinSpec extends AoCSpec {
  override def example: String =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin

  override def examplePartOne(): Any =
    SmokeBasin.one(exampleAsLines) shouldBe 15

  override def assignmentPartOne(): Any =
    SmokeBasin.one(inputAsLines) shouldBe 539

  override def examplePartTwo(): Any =
    SmokeBasin.two(exampleAsLines) shouldBe 1134

  override def assignmentPartTwo(): Any =
    SmokeBasin.two(inputAsLines) shouldBe 736920
}
