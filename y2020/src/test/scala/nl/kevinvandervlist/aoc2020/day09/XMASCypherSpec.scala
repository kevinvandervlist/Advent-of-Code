package nl.kevinvandervlist.aoc2020.day09

import nl.kevinvandervlist.aoc.AoCSpec

class XMASCypherSpec extends AoCSpec {
  override def example: String =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  override def examplePartOne(): Any =
    XMASCypher.one(exampleAsLines, 5) shouldBe 127

  override def assignmentPartOne(): Any =
    XMASCypher.one(inputAsLines, 25) shouldBe 552655238

  override def examplePartTwo(): Any =
    XMASCypher.two(exampleAsLines, 5) shouldBe 62

  override def assignmentPartTwo(): Any = {
    val result = XMASCypher.two(inputAsLines, 25)
    result should not be 75253258
    result should not be 92420996071548L
    result shouldBe 70672245
  }
}
