package nl.kevinvandervlist.aoc2021.day02

import nl.kevinvandervlist.aoc.AoCSpec
import nl.kevinvandervlist.aoc2021.day02.Dive

class DiveSpec extends AoCSpec {
  override def example: String =
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2
      |""".stripMargin

  override def examplePartOne(): Any =
    Dive.one(exampleAsLines) shouldBe 150

  override def assignmentPartOne(): Any =
    Dive.one(inputAsLines) shouldBe 1694130

  override def examplePartTwo(): Any =
    Dive.two(exampleAsLines) shouldBe 900

  override def assignmentPartTwo(): Any =
    Dive.two(inputAsLines) shouldBe 1698850445
}
