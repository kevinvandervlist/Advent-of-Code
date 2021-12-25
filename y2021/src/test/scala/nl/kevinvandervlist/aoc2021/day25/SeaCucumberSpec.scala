package nl.kevinvandervlist.aoc2021.day25

import nl.kevinvandervlist.aoc.AoCSpec

class SeaCucumberSpec extends AoCSpec {
  override def example: String =
    """v...>>.vv>
      |.vv>>.vv..
      |>>.>v>...v
      |>>v>>.>.v.
      |v>v.vv.v..
      |>.>>..v...
      |.vv..>.>v.
      |v.v..>>v.v
      |....v..v.>""".stripMargin

  override def examplePartOne(): Any =
    SeaCucumber.one(exampleAsLines) shouldBe 58

  override def assignmentPartOne(): Any =
    SeaCucumber.one(inputAsLines) shouldBe 321

  override def examplePartTwo(): Any = ()

  override def assignmentPartTwo(): Any = ()
}
