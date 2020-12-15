package nl.kevinvandervlist.aoc2015.day07

import nl.kevinvandervlist.aoc.AoCSpec

class AssemblyRequiredSpec extends AoCSpec {
  override def example: String =
    """123 -> x
      |456 -> y
      |x AND y -> d
      |x OR y -> e
      |x LSHIFT 2 -> f
      |y RSHIFT 2 -> g
      |NOT x -> h
      |NOT y -> i
      |""".stripMargin

  override def examplePartOne(): Any = {
    val result = AssemblyRequired.one(exampleAsLines)
    result.getOrElse("d", 0) shouldBe 72
    result.getOrElse("e", 0) shouldBe 507
    result.getOrElse("f", 0) shouldBe 492
    result.getOrElse("g", 0) shouldBe 114
    result.getOrElse("h", 0) shouldBe 65412
    result.getOrElse("i", 0) shouldBe 65079
    result.getOrElse("x", 0) shouldBe 123
    result.getOrElse("y", 0) shouldBe 456
  }

  override def assignmentPartOne(): Any =
    AssemblyRequired.one(inputAsLines).getOrElse("a", 0) shouldBe 3176

  override def examplePartTwo(): Any = {}

  override def assignmentPartTwo(): Any = {
    AssemblyRequired.two(inputAsLines) should not be 3176
    AssemblyRequired.two(inputAsLines) shouldBe 14710
  }
}
