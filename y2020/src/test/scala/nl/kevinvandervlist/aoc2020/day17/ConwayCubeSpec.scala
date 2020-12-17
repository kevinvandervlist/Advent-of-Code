package nl.kevinvandervlist.aoc2020.day17

import nl.kevinvandervlist.aoc.AoCSpec

class ConwayCubeSpec extends AoCSpec {
  override def example: String =
    """.#.
      |..#
      |###
      |""".stripMargin

  override def examplePartOne(): Any =
    ConwayCube.one(exampleAsLines) shouldBe 112

  override def assignmentPartOne(): Any =
    ConwayCube.one(inputAsLines) shouldBe 375

  override def examplePartTwo(): Any =
    ConwayCube.two(exampleAsLines) shouldBe 848

  override def assignmentPartTwo(): Any =
    ConwayCube.two(inputAsLines) shouldBe 2192
}
