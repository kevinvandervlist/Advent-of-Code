package nl.kevinvandervlist.aoc2021.day13

import nl.kevinvandervlist.aoc.AoCSpec

class TransparentOrigamiSpec extends AoCSpec {
  override def example: String =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5""".stripMargin

  override def examplePartOne(): Any =
    TransparentOrigami.one(exampleAsLines) shouldBe 17

  override def assignmentPartOne(): Any =
    TransparentOrigami.one(inputAsLines) shouldBe 781

  override def examplePartTwo(): Any =
    TransparentOrigami.two(exampleAsLines) shouldBe
      """█████
        |█░░░█
        |█░░░█
        |█░░░█
        |█████
        |░░░░░
        |░░░░░""".stripMargin

  override def assignmentPartTwo(): Any =
    TransparentOrigami.two(inputAsLines) shouldBe
      """███░░████░███░░░██░░░██░░░░██░███░░███░░
        |█░░█░█░░░░█░░█░█░░█░█░░█░░░░█░█░░█░█░░█░
        |█░░█░███░░█░░█░█░░░░█░░░░░░░█░█░░█░███░░
        |███░░█░░░░███░░█░░░░█░██░░░░█░███░░█░░█░
        |█░░░░█░░░░█░█░░█░░█░█░░█░█░░█░█░░░░█░░█░
        |█░░░░████░█░░█░░██░░░███░░██░░█░░░░███░░""".stripMargin
}
