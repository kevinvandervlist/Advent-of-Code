package nl.kevinvandervlist.aoc2020.day01

import nl.kevinvandervlist.aoc.AoCSpec
import nl.kevinvandervlist.aoc2021.day01.SonarSweep

class SonarSweepSpec extends AoCSpec {
  override def example: String =
    """199
      |200
      |208
      |210
      |200
      |207
      |240
      |269
      |260
      |263
      |""".stripMargin

  override def examplePartOne(): Any =
    SonarSweep.one(exampleAsLines) shouldBe 7

  override def assignmentPartOne(): Any =
    SonarSweep.one(inputAsLines) shouldBe 1715

  override def examplePartTwo(): Any =
    SonarSweep.two(exampleAsLines) shouldBe 5

  override def assignmentPartTwo(): Any =
    SonarSweep.two(inputAsLines) shouldBe 1739
}
