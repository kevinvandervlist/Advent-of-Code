package nl.kevinvandervlist.aoc2021.day15

import nl.kevinvandervlist.aoc.{Point, RectangularGrid}

object Chiton {
  def one(in: List[String]): Int =
    findMin(RectangularGrid.fromSingleDigitGrid(in))

  def two(in :List[String]): Int =
    findMin(expand(RectangularGrid.fromSingleDigitGrid(in)))

  def findMin(grid: RectangularGrid[Int]): Int = {
    val target = Point(grid.width - 1, grid.height - 1)
    val cheapestPath = grid.aStar(
      start = Point(0,0),
      target = target,
      heuristic = p => (target.x - p.x) + (target.y - p.y), // simple manhattan distance
      weight = identity
    )
    cheapestPath.drop(1).map(grid.get).collect { case Some(x) => x }.sum
  }

  def expand(original: RectangularGrid[Int]): RectangularGrid[Int] = {
    val topRow = (1 to 4).foldLeft(original) {
      case (g, increment) =>
        val appendRight = original.copy(original.elements.map(_.map {
          case n if n + increment > 9 => (n + increment) % 9
          case n => n + increment
        }))
        RectangularGrid(g.elements.zipWithIndex.map {
          case (row, idx) => row ++ appendRight.elements(idx)
        })
    }
    (1 to 4).foldLeft(topRow) {
      case (g, increment) =>
        val appendBottom = topRow.copy(topRow.elements.map(_.map {
          case n if n + increment > 9 => (n + increment) % 9
          case n => n + increment
        }))
        RectangularGrid(g.elements ++ appendBottom.elements)
    }
  }
}
