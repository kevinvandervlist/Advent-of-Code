package nl.kevinvandervlist.aoc2020.day14

import nl.kevinvandervlist.aoc.AoCSpec

class DockingDataSpec extends AoCSpec {
  override def example: String =
    """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
      |mem[8] = 11
      |mem[7] = 101
      |mem[8] = 0
      |""".stripMargin

  def example2: String =
    """mask = 000000000000000000000000000000X1001X
      |mem[42] = 100
      |mask = 00000000000000000000000000000000X0XX
      |mem[26] = 1
      |""".stripMargin

  override def examplePartOne(): Any =
    DockingData.one(exampleAsLines) shouldBe 165

  override def assignmentPartOne(): Any =
    DockingData.one(inputAsLines) shouldBe 8471403462063L

  override def examplePartTwo(): Any =
    DockingData.two(example2.split('\n').toList) shouldBe 208

  override def assignmentPartTwo(): Any =
    DockingData.two(inputAsLines) shouldBe 2667858637669L
}
