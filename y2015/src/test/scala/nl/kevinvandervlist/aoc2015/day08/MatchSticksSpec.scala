package nl.kevinvandervlist.aoc2015.day08

import nl.kevinvandervlist.aoc.AoCSpec

class MatchSticksSpec extends AoCSpec {
  override def example: String =
    """""
      |"abc"
      |"aaa\"aaa"
      |"\x27"
      |""".stripMargin

  override def examplePartOne(): Any =
    MatchSticks.one(exampleAsLines) shouldBe 12

  override def assignmentPartOne(): Any = {
    val result = MatchSticks.one(inputAsLines)
    result should not be 1096
    result should not be 1345
    result shouldBe 1342
  }

  override def examplePartTwo(): Any =
    MatchSticks.two(exampleAsLines) shouldBe 19

  override def assignmentPartTwo(): Any =
    MatchSticks.two(inputAsLines) shouldBe 2074
}
