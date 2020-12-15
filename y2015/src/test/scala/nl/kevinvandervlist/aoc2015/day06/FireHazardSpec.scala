package nl.kevinvandervlist.aoc2015.day06

import nl.kevinvandervlist.aoc.AoCSpec

class FireHazardSpec extends AoCSpec {
  override def example: String =
    """turn on 0,0 through 999,999
      |toggle 0,0 through 999,0
      |turn off 499,499 through 500,500
      |""".stripMargin

  override def examplePartOne(): Any =
    FireHazard.one(exampleAsLines) shouldBe (1000 * 1000 - 1000 - 4)

  override def assignmentPartOne(): Any =
    FireHazard.one(inputAsLines) shouldBe 377891

  override def examplePartTwo(): Any = {
    FireHazard.two(List("turn on 0,0 through 0,0")) shouldBe 1
    FireHazard.two(List("toggle 0,0 through 999,999")) shouldBe 2000000
  }

  override def assignmentPartTwo(): Any = {
    FireHazard.two(inputAsLines) should not be 25322127
    FireHazard.two(inputAsLines) shouldBe 14110788
  }
}
