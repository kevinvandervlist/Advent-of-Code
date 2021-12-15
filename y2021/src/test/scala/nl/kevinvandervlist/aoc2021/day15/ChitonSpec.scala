package nl.kevinvandervlist.aoc2021.day15

import nl.kevinvandervlist.aoc.{AoCSpec, RectangularGrid}

class ChitonSpec extends AoCSpec {
  override def example: String =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin

  override def examplePartOne(): Any =
    Chiton.one(exampleAsLines) shouldBe 40

  override def assignmentPartOne(): Any =
    Chiton.one(inputAsLines) shouldBe 707

  override def examplePartTwo(): Any =
    Chiton.two(exampleAsLines) shouldBe 315

  override def assignmentPartTwo(): Any = 
    Chiton.two(inputAsLines) shouldBe 2942
}
