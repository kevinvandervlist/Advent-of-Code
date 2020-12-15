package nl.kevinvandervlist.aoc2015.day05

import nl.kevinvandervlist.aoc.AoCSpec

class NaughtyNiceSpec extends AoCSpec {
  override def example: String =
    """ugknbfddgicrmopn
      |jchzalrnumimnmhp
      |haegwjzuvuyypxyu
      |dvszwmarrgswjxmb
      |""".stripMargin

  def example2: String =
    """qjhvhtzxzqqjkmpb
      |xxyxx
      |uurcxstgmygtbstg
      |ieodomkazucvgmuy
      |""".stripMargin

  override def examplePartOne(): Any =
    NaughtyNice.one(exampleAsLines) shouldBe 1

  override def assignmentPartOne(): Any =
    NaughtyNice.one(inputAsLines) shouldBe 238

  override def examplePartTwo(): Any =
    NaughtyNice.two(example2.split('\n').toList) shouldBe 2

  override def assignmentPartTwo(): Any = {
    NaughtyNice.two(inputAsLines) should not be 66
    NaughtyNice.two(inputAsLines) shouldBe 69
  }
}
