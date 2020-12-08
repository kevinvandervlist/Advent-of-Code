package nl.kevinvandervlist.aoc2020.day03

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class TreeSlopeSpec extends AnyWordSpec with Matchers {
  private val example =
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

  private val input = Source.fromResource("day-03-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      TreeSlope.countTrees(example.split('\n').toList) shouldBe 7
    }
    "validate assignment" in {
      TreeSlope.countTrees(input) shouldBe 184
    }
  }

  "Part 2" should {
    "validate example" in {
      TreeSlope.countTrees2(example.split('\n').toList) shouldBe List(2, 7, 3, 4, 2)
      TreeSlope.countTrees2(example.split('\n').toList).product shouldBe 336L
    }
    "validate assignment" in {
      println(TreeSlope.countTrees2(input))
      TreeSlope.countTrees2(input) should not be List(62, 184, 80, 74, 62)
      TreeSlope.countTrees2(input).product should not be 4187192320L

      TreeSlope.countTrees2(input) shouldBe List(62, 184, 80, 74, 36)
      TreeSlope.countTrees2(input).product shouldBe 2431272960L
    }
  }
}
