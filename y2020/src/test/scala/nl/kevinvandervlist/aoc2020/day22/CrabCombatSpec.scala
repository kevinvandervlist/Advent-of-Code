package nl.kevinvandervlist.aoc2020.day22

import nl.kevinvandervlist.aoc.AoCSpec

class CrabCombatSpec extends AoCSpec {
  override def example: String =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin

  override def examplePartOne(): Any =
    CrabCombat.one(exampleAsLines) shouldBe 306

  override def assignmentPartOne(): Any =
    CrabCombat.one(inputAsLines) shouldBe 31809

  override def examplePartTwo(): Any =
    CrabCombat.two(exampleAsLines) shouldBe 291

  override def assignmentPartTwo(): Any =
    CrabCombat.two(inputAsLines) shouldBe 32835
}
