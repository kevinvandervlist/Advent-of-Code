package nl.kevinvandervlist.aoc2018.day23

import nl.kevinvandervlist.aoc.AoCSpec

class TeleportationSpec extends AoCSpec {
  override def example: String =
    """pos=<0,0,0>, r=4
      |pos=<1,0,0>, r=1
      |pos=<4,0,0>, r=3
      |pos=<0,2,0>, r=1
      |pos=<0,5,0>, r=3
      |pos=<0,0,3>, r=1
      |pos=<1,1,1>, r=1
      |pos=<1,1,2>, r=1
      |pos=<1,3,1>, r=1
      |""".stripMargin

  override def examplePartOne(): Any =
    Teleportation.one(exampleAsLines) shouldBe 0

  override def assignmentPartOne(): Any =
    Teleportation.one(inputAsLines) shouldBe 0

  override def examplePartTwo(): Any =
    Teleportation.two(exampleAsLines) shouldBe 2208

  override def assignmentPartTwo(): Any = {
    Teleportation.two(inputAsLines) should be > 3607
  }
}
