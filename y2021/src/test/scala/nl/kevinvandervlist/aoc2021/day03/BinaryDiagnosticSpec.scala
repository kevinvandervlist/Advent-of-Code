package nl.kevinvandervlist.aoc2021.day03

import nl.kevinvandervlist.aoc.AoCSpec

class BinaryDiagnosticSpec extends AoCSpec {
  override def example: String =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010
      |""".stripMargin

  override def examplePartOne(): Any =
    BinaryDiagnostic.one(exampleAsLines) shouldBe 198

  override def assignmentPartOne(): Any =
    BinaryDiagnostic.one(inputAsLines) shouldBe 3429254

  override def examplePartTwo(): Any =
    BinaryDiagnostic.two(exampleAsLines) shouldBe 230

  override def assignmentPartTwo(): Any =
    BinaryDiagnostic.two(inputAsLines) shouldBe 5410338
}
