package nl.kevinvandervlist.aoc2020.day03

import nl.kevinvandervlist.aoc.AoCSpec

class TreeSlopeSpec extends AoCSpec {
  override def example: String =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#
      |""".stripMargin

  override def examplePartOne(): Any =
    TreeSlope.countTrees(exampleAsLines) shouldBe 7

  override def assignmentPartOne(): Any = {
    TreeSlope.countTrees(inputAsLines) shouldBe 184
  }

  override def examplePartTwo(): Any = {
    TreeSlope.countTrees2(exampleAsLines) shouldBe List(2, 7, 3, 4, 2)
    TreeSlope.countTrees2(exampleAsLines).product shouldBe 336L
  }

  override def assignmentPartTwo(): Any = {
    TreeSlope.countTrees2(inputAsLines) should not be List(62, 184, 80, 74, 62)
    TreeSlope.countTrees2(inputAsLines).product should not be 4187192320L

    TreeSlope.countTrees2(inputAsLines) shouldBe List(62, 184, 80, 74, 36)
    TreeSlope.countTrees2(inputAsLines).product shouldBe 2431272960L
  }
}
