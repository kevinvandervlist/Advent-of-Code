package nl.kevinvandervlist.aoc2020.day23

import nl.kevinvandervlist.aoc.AoCSpec

class CrabCupsSpec extends AoCSpec {
  override def example: String =
    """389125467""".stripMargin

  override def examplePartOne(): Any =
    CrabCups.one(example) shouldBe "67384529"

  override def assignmentPartOne(): Any =
    CrabCups.one(inputAsString) shouldBe "69425837"

  override def examplePartTwo(): Any =
    CrabCups.two(example) shouldBe "149245887792"

  override def assignmentPartTwo(): Any =
    CrabCups.two(inputAsString) shouldBe "218882971435"
}
