package nl.kevinvandervlist.aoc2021.day11

import nl.kevinvandervlist.aoc.RectangularGrid

object DomboOctopuses {
  def one(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    val grid = RectangularGrid.applyInnerArray(nums)
    val res = (1 to 100).foldLeft(grid -> 0) {
      case ((g, c), _) => step(g, c)
    }
    res._2
  }
  def two(in: List[String]): Int = {
    val nums = in.map(_.toCharArray.map(_.toString.toInt))
    var grid = RectangularGrid.applyInnerArray(nums)
    var cnt = 0
    while(true) {
      cnt += 1
      val res = step(grid, 0)
      grid = res._1
      if(res._2 == nums.length * nums.head.length) {
        return cnt
      }
    }
    throw new IllegalStateException("Should terminate")
  }

  private def step(grid: RectangularGrid[Int], flashes: Int): (RectangularGrid[Int], Int) = {
    var updatedGrid = grid
    // all +1
    grid.allCoordinates.foreach {
      case (x, y) =>
        updatedGrid = updatedGrid.set(x, y, n => n + 1)
    }
    // check who flashes
    var toFlash: List[(Int, Int)] = updatedGrid
      .allCoordinates
      .filter(c => updatedGrid.get(c).get > 9)
      .toList

    while(toFlash.nonEmpty) {
      val c = toFlash.head
      toFlash = toFlash.tail
      val diags = updatedGrid.getSquaredDiagonalNeighbouringCoordinates(c).toVector
      diags.foreach { nc =>
        if(updatedGrid.get(nc).get == 9) {
          toFlash = nc :: toFlash
        }
        updatedGrid = updatedGrid.set(nc, n => n + 1)
      }
    }

    // finally reset all flashed nodes and count how often that occurred.
    var flashCount = flashes
    updatedGrid
      .allCoordinates
      .filter(c => updatedGrid.get(c).get > 9)
      .foreach {
        case (x, y) =>
          flashCount += 1
          updatedGrid = updatedGrid.set(x, y, n => 0)
      }

    updatedGrid -> flashCount
  }
}
