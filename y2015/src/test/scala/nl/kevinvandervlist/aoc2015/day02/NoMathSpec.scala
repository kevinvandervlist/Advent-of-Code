package nl.kevinvandervlist.aoc2015.day02

import nl.kevinvandervlist.aoc.AoCSpec

class NoMathSpec extends AoCSpec {
  override def example: String =
    """2x3x4
      |1x1x10
      |""".stripMargin

  override def examplePartOne(): Any =
    NoMath.one(exampleAsLines) shouldBe 101

  override def assignmentPartOne(): Any =
    NoMath.one(inputAsLines) shouldBe 1588178

  override def examplePartTwo(): Any =
    NoMath.two(exampleAsLines) shouldBe 48

  override def assignmentPartTwo(): Any = {
    NoMath.two(inputAsLines) should not be 3782116
    NoMath.two(inputAsLines) shouldBe 3783758
  }
}
