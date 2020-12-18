package nl.kevinvandervlist.aoc2020.day18

import nl.kevinvandervlist.aoc.AoCSpec

class OperationOrderSpec extends AoCSpec {
  override def example: String =
    """1 + (2 * 3) + (4 * (5 + 6))
      |2 * 3 + (4 * 5)
      |5 + (8 * 3 + 9 + 3 * 4 * 3)
      |5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
      |((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
      |""".stripMargin

  override def examplePartOne(): Any = {
    val expected: Long = 51 + 26 + 437 + 12240 + 13632
    OperationOrder.one(exampleAsLines) shouldBe expected
  }

  override def assignmentPartOne(): Any = {
    val result = OperationOrder.one(inputAsLines)
    result should not be 11650962581L
    result shouldBe 21022630974613L
  }

  override def examplePartTwo(): Any = {
    val expected = 51 + 46 + 1445 + 669060 + 23340
    OperationOrder.two(exampleAsLines) shouldBe expected
  }

  override def assignmentPartTwo(): Any =
    OperationOrder.two(inputAsLines) shouldBe 169899524778212L
}
