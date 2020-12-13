package nl.kevinvandervlist.aoc2020.day13

import nl.kevinvandervlist.aoc.AoCSpec

class ShuttleSearchSpec extends AoCSpec {
  override def example: String =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  override def examplePartOne(): Any =
    ShuttleSearch.one(exampleAsLines) shouldBe 295

  override def assignmentPartOne(): Any =
    ShuttleSearch.one(inputAsLines) shouldBe 171

  override def examplePartTwo(): Any = {
    ShuttleSearch.two(0, exampleAsLines) shouldBe 1068781
    ShuttleSearch.two(0, List("0", "17,x,13,19")) shouldBe 3417
    ShuttleSearch.two(0, List("0", "67,7,59,61")) shouldBe 754018
    ShuttleSearch.two(0, List("0", "67,x,7,59,61")) shouldBe 779210
    ShuttleSearch.two(0, List("0", "67,7,x,59,61")) shouldBe 1261476
    ShuttleSearch.two(0, List("0", "1789,37,47,1889")) shouldBe 1202161486
  }

  override def assignmentPartTwo(): Any =
    ShuttleSearch.two(0, inputAsLines) shouldBe 539746751134958L
}
