package nl.kevinvandervlist.aoc2021.day09

import nl.kevinvandervlist.aoc.SquareGrid

object SmokeBasin {
  def one(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    val grid = SquareGrid.applyInnerArray(nums)
    getLowPoints(grid).map(grid.get).map(h => 1 + h.get).sum
  }

  def two(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    val grid = SquareGrid.applyInnerArray(nums)
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

  private def exploreBasin(grid: SquareGrid[Int], lowPoint: (Int, Int)): Set[(Int, Int)] = {
    // A basin should only contain unique elements, and we fill it by just exploring the map
    // according to the given criteria
    var basin = Set.empty[(Int, Int)]
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

  private def getLowPoints(grid: SquareGrid[Int]): Iterable[(Int, Int)] = grid.allCoordinates.filter {
    case (x, y) =>
      val neighbouringCoordinates = grid
        .getSquaredNeighbouringCoordinates(x, y)
        .map(grid.get)
        .collect { case Some(x) => x }
      neighbouringCoordinates.min > grid.get(x, y).get
  }
}
