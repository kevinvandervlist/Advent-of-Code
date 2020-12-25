package nl.kevinvandervlist.aoc2020.day25

import nl.kevinvandervlist.aoc.AoCSpec

class ComboBreakerSpec extends AoCSpec {
  override def example: String =
    """5764801
      |17807724""".stripMargin

  override def examplePartOne(): Any =
    ComboBreaker.one(exampleAsLines) shouldBe 14897079

  override def assignmentPartOne(): Any =
    ComboBreaker.one(inputAsLines) shouldBe 4441893

  override def examplePartTwo(): Any = {}

  override def assignmentPartTwo(): Any = {}
}
