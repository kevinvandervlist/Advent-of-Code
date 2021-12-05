package nl.kevinvandervlist.aoc2021.day05

import nl.kevinvandervlist.aoc.AoCSpec

class HydrothermalVenturSpec extends AoCSpec {
  override def example: String =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2
      |""".stripMargin

  override def examplePartOne(): Any =
    HydrothermalVenture.one(exampleAsLines) shouldBe 5

  override def assignmentPartOne(): Any =
    HydrothermalVenture.one(inputAsLines) shouldBe 6841

  override def examplePartTwo(): Any =
    HydrothermalVenture.two(exampleAsLines) shouldBe 12

  override def assignmentPartTwo(): Any =
    HydrothermalVenture.two(inputAsLines) shouldBe 19258
}
