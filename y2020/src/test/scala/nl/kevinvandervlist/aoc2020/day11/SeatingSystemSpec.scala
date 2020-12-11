package nl.kevinvandervlist.aoc2020.day11

import nl.kevinvandervlist.aoc.AoCSpec

class SeatingSystemSpec extends AoCSpec {
  override def example: String =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL
      |""".stripMargin

  override def examplePartOne(): Any =
    SeatingSystem.one(exampleAsLines) shouldBe 37

  override def assignmentPartOne(): Any = {
    val result = SeatingSystem.one(inputAsLines)
    result should not be 2291
    result shouldBe 2289
  }

  override def examplePartTwo(): Any =
    SeatingSystem.two(exampleAsLines) shouldBe 26

  override def assignmentPartTwo(): Any =
    SeatingSystem.two(inputAsLines) shouldBe 2059
}
