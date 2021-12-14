package nl.kevinvandervlist.aoc2021.day14

import nl.kevinvandervlist.aoc.AoCSpec

class ExtendedPolymerisationSpec extends AoCSpec {
  override def example: String =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin

  override def examplePartOne(): Any =
    ExtendedPolymerisation.one(exampleAsLines) shouldBe 1588

  override def assignmentPartOne(): Any =
    ExtendedPolymerisation.one(inputAsLines) shouldBe 2003

  override def examplePartTwo(): Any =
    ExtendedPolymerisation.two(exampleAsLines) shouldBe 2188189693529L

  override def assignmentPartTwo(): Any =
    ExtendedPolymerisation.two(inputAsLines) shouldBe 2276644000111L
}
