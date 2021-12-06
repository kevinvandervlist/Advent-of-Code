package nl.kevinvandervlist.aoc2021.day06

import nl.kevinvandervlist.aoc.AoCSpec

class LanternFishSpec extends AoCSpec {
  override def example: String =
    """3,4,3,1,2""".stripMargin

  override def examplePartOne(): Any =
    LanternFish.one(example) shouldBe 5934

  override def assignmentPartOne(): Any =
    LanternFish.one(inputAsString) shouldBe 383160

  override def examplePartTwo(): Any =
    LanternFish.two(example) shouldBe 26984457539L

  override def assignmentPartTwo(): Any =
    LanternFish.two(inputAsString) shouldBe 1721148811504L
}
