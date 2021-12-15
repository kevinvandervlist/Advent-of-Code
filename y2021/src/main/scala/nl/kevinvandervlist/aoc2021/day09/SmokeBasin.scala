package nl.kevinvandervlist.aoc2021.day09

import nl.kevinvandervlist.aoc.{Point, RectangularGrid}

object SmokeBasin {
  def one(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    val grid = RectangularGrid.applyInnerArray(nums)
    getLowPoints(grid).map(grid.get).map(h => 1 + h.get).sum
  }

  def two(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    val grid = RectangularGrid.applyInnerArray(nums)
    val lowPoints = getLowPoints(grid)
    val basins = lowPoints
      .map(exploreBasin(grid, _))
      .map(_.size)
    basins
      .toList
      .sorted
      .reverse
      .take(3)
      .product
  }

  private def exploreBasin(grid: RectangularGrid[Int], lowPoint: Point): Set[Point] = {
    // A basin should only contain unique elements, and we fill it by just exploring the map
    // according to the given criteria
    var basin = Set.empty[Point]
    var stack = List(lowPoint)
    while(stack.nonEmpty) {
      basin = basin + stack.head

      val currentCoordinate = stack.head
      stack = stack.tail
      val currentValue = grid.get(currentCoordinate).get

      grid
        .getSquaredNeighbouringCoordinates(currentCoordinate)
        .foreach { neighbour =>
          val neighbouringValue = grid.get(neighbour).get
          if (neighbouringValue != 9 && neighbouringValue > currentValue) {
            stack = stack :+ neighbour
          }
        }
    }
    basin
  }

  private def getLowPoints(grid: RectangularGrid[Int]): Iterable[Point] = grid.allCoordinates.filter {
    case Point(x, y) =>
      val neighbouringCoordinates = grid
        .getSquaredNeighbouringCoordinates(x, y)
        .map(grid.get)
        .collect { case Some(x) => x }
      neighbouringCoordinates.min > grid.get(x, y).get
  }
}
