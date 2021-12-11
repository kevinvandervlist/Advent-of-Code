package nl.kevinvandervlist.aoc2021.day11

import nl.kevinvandervlist.aoc.AoCSpec

class DomboOctopusesSpec extends AoCSpec {
  override def example: String =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  override def examplePartOne(): Any =
    DomboOctopuses.one(exampleAsLines) shouldBe 1656

  override def assignmentPartOne(): Any =
    DomboOctopuses.one(inputAsLines) shouldBe 1793

  override def examplePartTwo(): Any =
    DomboOctopuses.two(exampleAsLines) shouldBe 195

  override def assignmentPartTwo(): Any =
    DomboOctopuses.two(inputAsLines) shouldBe 247

}