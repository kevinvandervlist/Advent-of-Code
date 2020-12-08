package nl.kevinvandervlist.aoc2020.day08

import nl.kevinvandervlist.aoc.AoCSpec

class HandheldHaltingSpec extends AoCSpec {
  override def example: String =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6
      |""".stripMargin

  override def examplePartOne(): Any =
    HandheldHalting.one(exampleAsLines) shouldBe 5

  override def assignmentPartOne(): Any =
    HandheldHalting.one(inputAsLines) shouldBe 1723

  override def examplePartTwo(): Any =
    HandheldHalting.two(exampleAsLines) shouldBe 8

  override def assignmentPartTwo(): Any =
    HandheldHalting.two(inputAsLines) shouldBe 846
}
