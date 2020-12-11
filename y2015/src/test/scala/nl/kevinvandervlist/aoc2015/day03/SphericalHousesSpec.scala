package nl.kevinvandervlist.aoc2015.day03

import nl.kevinvandervlist.aoc.AoCSpec

class SphericalHousesSpec extends AoCSpec {
  override def example: String =
    """>
      |^>v<
      |^v^v^v^v^v
      |""".stripMargin

  override def examplePartOne(): Any =
    SphericalHouses.one(exampleAsLines) shouldBe 8

  override def assignmentPartOne(): Any =
    SphericalHouses.one(inputAsLines) shouldBe 2081

  override def examplePartTwo(): Any = {
    SphericalHouses.two("^v") shouldBe 3
    SphericalHouses.two("^>v<") shouldBe 3
    SphericalHouses.two("^v^v^v^v^v") shouldBe 11
  }

  override def assignmentPartTwo(): Any =
    SphericalHouses.two(inputAsString) shouldBe 2341
}
