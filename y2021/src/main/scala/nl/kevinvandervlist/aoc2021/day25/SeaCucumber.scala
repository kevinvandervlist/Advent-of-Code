package nl.kevinvandervlist.aoc2021.day25

import nl.kevinvandervlist.aoc.{Point, RectangularGrid}

object SeaCucumber {
  def one(in: List[String]): Int = {
    var currentGrid = RectangularGrid.fromSingleCharGrid(in)
    val grids = LazyList.continually {
      currentGrid = moveVerticals(moveHorizontals(currentGrid))
      currentGrid
    }
    LazyList.from(1).zip(grids.zip(grids.tail)).dropWhile {
      case (iteration, (before, after)) => before != after
    }.head._1 + 1 // we drop the 'final' one, so adjust accordingly
  }

  private def move(currentGrid: RectangularGrid[Char], adjustCoordinate: Point => Point, cucumber: Char): RectangularGrid[Char] = currentGrid
    .allCoordinates
    .map(c => c -> adjustCoordinate(c))
    .filter { case (before, after) =>
      currentGrid.get(before).contains(cucumber) && currentGrid.get(after).contains('.')
    }.foldLeft(currentGrid) {
      case (g, (before, after)) => g.set(before, _ => '.').set(after, _ => cucumber)
    }

  private def moveHorizontals(currentGrid: RectangularGrid[Char]): RectangularGrid[Char] =
    move(currentGrid, c => c.copy(x = (c.x + 1) % currentGrid.width), '>')

  private def moveVerticals(currentGrid: RectangularGrid[Char]): RectangularGrid[Char] =
    move(currentGrid, c => c.copy(y = (c.y + 1) % currentGrid.height), 'v')
}
