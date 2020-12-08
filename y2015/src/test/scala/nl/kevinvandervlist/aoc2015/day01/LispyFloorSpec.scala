package nl.kevinvandervlist.aoc2015.day01

import nl.kevinvandervlist.aoc.AoCSpec

class LispyFloorSpec extends AoCSpec {
  override def example: String =
    """(())
      |()()
      |(((
      |(()(()(
      |))(((((
      |())
      |))(
      |)))
      |)())())
      |""".stripMargin

  override def examplePartOne(): Any = {
    val splitted = example.split('\n')
    LispyFloor.one(splitted(0)) shouldBe 0
    LispyFloor.one(splitted(1)) shouldBe 0
    LispyFloor.one(splitted(2)) shouldBe 3
    LispyFloor.one(splitted(3)) shouldBe 3
    LispyFloor.one(splitted(4)) shouldBe 3
    LispyFloor.one(splitted(5)) shouldBe -1
    LispyFloor.one(splitted(6)) shouldBe -1
    LispyFloor.one(splitted(7)) shouldBe -3
    LispyFloor.one(splitted(8)) shouldBe -3
  }

  override def assignmentPartOne(): Any =
    LispyFloor.one(inputAsString) shouldBe 74

  override def examplePartTwo(): Any = {
    val example =
      s""")
         |()())
         |""".stripMargin
    val splitted = example.split('\n')
    LispyFloor.two(splitted(0)) shouldBe 1
    LispyFloor.two(splitted(1)) shouldBe 5
  }

  override def assignmentPartTwo(): Any =
    LispyFloor.two(inputAsString) shouldBe 1795
}
