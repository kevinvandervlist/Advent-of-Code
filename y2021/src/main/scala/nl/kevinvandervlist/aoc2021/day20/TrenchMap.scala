package nl.kevinvandervlist.aoc2021.day20

import nl.kevinvandervlist.aoc.{Binary, Characters, InfiniteRectangularGrid, Point}

object TrenchMap {
  def one(in: List[String]): Int = {
    val img = repeat(in, 2)
    img.allCoordinates.map(c => img.get(c)).count(_ == Characters.SQUARE)
  }

  def two(in: List[String]): Int = {
    val img = repeat(in, 50)
    img.allCoordinates.map(c => img.get(c)).count(_ == Characters.SQUARE)
  }

  private def repeat(in: List[String], times: Int): InfiniteRectangularGrid[Char] = {
    val improved = in.map(_.replace('.', Characters.EMPTY).replace('#', Characters.SQUARE))
    val tm = InfiniteRectangularGrid[Char]
    val dec = decoder(improved.head)

    // Aah infinity boundary toggles between on and off per generation...
    val even = () => Characters.EMPTY
    val odd = if(dec("000000000") == Characters.SQUARE && dec("111111111") == Characters.EMPTY)
      () => Characters.SQUARE
    else () => Characters.EMPTY

    val grid = InfiniteRectangularGrid.fromGrid(improved.tail.tail.map(_.toList), even)

    (1 to times).foldLeft(grid) {
      case (g, n) if n % 2 == 0 => next(g, dec).copy(nothing = even)
      case (g, n) => next(g, dec).copy(nothing = odd)
    }
  }

  private def next(currentGrid: InfiniteRectangularGrid[Char], dec: Map[String, Char]): InfiniteRectangularGrid[Char] = {
    // add another 'layer' of the image to enlarge it incrementally.
    val newGrid = Set(
      ((currentGrid.xMin - 1) to (currentGrid.xMax + 1)).map(x => Point(x, currentGrid.yMin - 1)), // top
      ((currentGrid.xMin - 1) to (currentGrid.xMax + 1)).map(x => Point(x, currentGrid.yMax + 1)), // bottom
      ((currentGrid.yMin - 1) to (currentGrid.yMax + 1)).map(y => Point(currentGrid.xMin - 1, y)), // left
      ((currentGrid.yMin - 1) to (currentGrid.yMax + 1)).map(y => Point(currentGrid.xMax + 1, y)), // right
    ).flatten.foldLeft(currentGrid) {
      case (g, p) => g.set(p, _ => g.nothing())
    }
    newGrid.allCoordinates.foldLeft(newGrid) {
      case (g, p) => g.set(p, _ => nextValueOf(p, currentGrid, dec))
    }
  }

  private def nextValueOf(p: Point, g: InfiniteRectangularGrid[Char], dec: Map[String, Char]): Char = {
    dec(g.getNineSquaresBasedOn(p).map(p => g.get(p)).map {
      case Characters.EMPTY => '0'
      case Characters.SQUARE => '1'
    }.mkString(""))
  }

  private def decoder(str: String): Map[String, Char] =
    str.zipWithIndex.map {
      case (c, idx) => Binary.toBinary(idx, 9) -> c
    }.toMap
}
