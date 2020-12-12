package nl.kevinvandervlist.aoc2020.day12

import nl.kevinvandervlist.aoc.AoCSpec

class RainRiskSpec extends AoCSpec {
  override def example: String =
    """F10
      |N3
      |F7
      |R90
      |F11
      |""".stripMargin

  override def examplePartOne(): Any =
    RainRisk.one(exampleAsLines) shouldBe 25

  override def assignmentPartOne(): Any =
    RainRisk.one(inputAsLines) shouldBe 1441

  override def examplePartTwo(): Any =
    RainRisk.two(exampleAsLines) shouldBe 286

  override def assignmentPartTwo(): Any = {
    val result = RainRisk.two(inputAsLines)
    result should not be 17492
    result should not be 18892
    result should not be 2090769004
    result shouldBe 61616
  }
}
