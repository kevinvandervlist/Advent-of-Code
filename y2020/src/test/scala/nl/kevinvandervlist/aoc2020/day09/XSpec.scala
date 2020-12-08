package nl.kevinvandervlist.aoc2020.day09

import nl.kevinvandervlist.aoc.AoCSpec

class XSpec extends AoCSpec {
  override def example: String =
    """
      |""".stripMargin

  override def examplePartOne(): Any =
    X.one(exampleAsLines) shouldBe -1

  override def assignmentPartOne(): Any =
    X.one(inputAsLines) shouldBe -1

  override def examplePartTwo(): Any =
    X.two(exampleAsLines) shouldBe -1

  override def assignmentPartTwo(): Any =
    X.two(inputAsLines) shouldBe -1
}
