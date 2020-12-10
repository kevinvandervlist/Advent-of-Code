package nl.kevinvandervlist.aoc2020.day10

import nl.kevinvandervlist.aoc.AoCSpec

class BatteryArraySpec extends AoCSpec {
  def smallExample: String =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  override def example: String =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  override def examplePartOne(): Any = {
    BatteryArray.one(smallExample.split('\n').toList) shouldBe 35
    BatteryArray.one(exampleAsLines) shouldBe 220
  }

  override def assignmentPartOne(): Any =
    BatteryArray.one(inputAsLines) shouldBe 2048

  override def examplePartTwo(): Any = {
    BatteryArray.two(smallExample.split('\n').toList) shouldBe 8
    BatteryArray.two(exampleAsLines) shouldBe 19208
  }

  override def assignmentPartTwo(): Any =
    BatteryArray.two(inputAsLines) shouldBe 1322306994176L
}
