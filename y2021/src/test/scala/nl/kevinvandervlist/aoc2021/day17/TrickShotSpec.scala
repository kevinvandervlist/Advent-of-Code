package nl.kevinvandervlist.aoc2021.day17

import nl.kevinvandervlist.aoc.AoCSpec

class TrickShotSpec extends AoCSpec {
  override def example: String =
    "target area: x=20..30, y=-10..-5"

  override def examplePartOne(): Any =
    TrickShot.one(example) shouldBe 45

  override def assignmentPartOne(): Any =
    TrickShot.one(inputAsString) shouldBe 6441

  override def examplePartTwo(): Any =
    TrickShot.two(example) shouldBe 112

  override def assignmentPartTwo(): Any =
    TrickShot.two(inputAsString) shouldBe 3186
}
