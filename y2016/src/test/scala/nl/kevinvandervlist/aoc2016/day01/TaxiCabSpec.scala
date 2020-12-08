package nl.kevinvandervlist.aoc2016.day01

import nl.kevinvandervlist.aoc.AoCSpec

class TaxiCabSpec extends AoCSpec {
  override def example: String =
    """R2, L3
      |R2, R2, R2
      |R5, L5, R5, R3
      |""".stripMargin

  override def examplePartOne(): Any = {
    val splitted = example.split('\n').toList
    TaxiCab.one(splitted(0)) shouldBe 5
    TaxiCab.one(splitted(1)) shouldBe 2
    TaxiCab.one(splitted(2)) shouldBe 12
  }

  override def assignmentPartOne(): Any =
    TaxiCab.one(inputAsString) shouldBe 299

  override def examplePartTwo(): Any =
    TaxiCab.two("R8, R4, R4, R8") shouldBe 4

  override def assignmentPartTwo(): Any =
    TaxiCab.two(inputAsString) shouldBe 181
}

