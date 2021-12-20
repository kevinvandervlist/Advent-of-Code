package nl.kevinvandervlist.aoc2021.day20

import nl.kevinvandervlist.aoc.AoCSpec

class TrenchMapSpec extends AoCSpec {
  override def example: String =
    """..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#
      |
      |#..#.
      |#....
      |##..#
      |..#..
      |..###""".stripMargin

  override def examplePartOne(): Any =
    TrenchMap.one(exampleAsLines) shouldBe 35

  override def assignmentPartOne(): Any =
    TrenchMap.one(inputAsLines) shouldBe 5419

  override def examplePartTwo(): Any =
    TrenchMap.two(exampleAsLines) shouldBe 3351

  override def assignmentPartTwo(): Any =
    TrenchMap.two(inputAsLines) shouldBe 17325
}
