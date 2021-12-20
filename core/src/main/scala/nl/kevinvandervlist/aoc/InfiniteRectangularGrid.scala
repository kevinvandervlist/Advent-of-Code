package nl.kevinvandervlist.aoc

import nl.kevinvandervlist.aoc.Point
import nl.kevinvandervlist.aoc.RectangularGrid

import scala.collection.mutable

object InfiniteRectangularGrid {
  def fromGrid[T](grid: List[List[T]], nothing: () => T): InfiniteRectangularGrid[T] = {
    val g = RectangularGrid[T](grid)
    val somethings = g.allCoordinates.map(c => c -> g.get(c).get).toMap
    new InfiniteRectangularGrid[T](somethings, nothing)
  }
}

case class InfiniteRectangularGrid[T](elements: Map[Point, T], nothing: () => T) {
  lazy private val asGrid = {
    // rebase coordinates based on (0, 0) as start
    val xOffset = if(xMin < 0) -1 * xMin else 0
    val yOffset = if(yMin < 0) -1 * yMin else 0
    elements.foldLeft(RectangularGrid.apply(width + 1, height + 1, nothing())) {
      case (g, (p, t)) => g.set(p.copy(x = p.x + xOffset, y = p.y + yOffset), _ => t)
    }
  }

  lazy val xMin: Int =
    elements.keys.minBy(_.x).x

  lazy val xMax: Int =
    elements.keys.maxBy(_.x).x

  lazy val width: Int =
    xMax - xMin

  lazy val yMin: Int =
    elements.keys.minBy(_.y).y

  lazy val yMax: Int =
    elements.keys.maxBy(_.y).y

  lazy val height: Int =
    yMax - yMin

  def print: String =
    asGrid.print

  def allCoordinates: Iterable[Point] =
    (yMin to yMax).flatMap(y => (xMin to xMax).map(x => Point(x, y)))

  def flipTop: InfiniteRectangularGrid[T] =
    ???

  def transpose: InfiniteRectangularGrid[T] =
    ???

  def flipLeft: InfiniteRectangularGrid[T] =
    ???

  def count(p: T => Boolean): Long =
    elements.values.count(p)

  def set(c: Point, f: T => T): InfiniteRectangularGrid[T] = {
    val old = get(c)
    val updated = f(get(c))
    copy(elements = elements + (c -> updated))
  }

  def set(x: Int, y: Int, f: T => T): InfiniteRectangularGrid[T] =
    set(Point(x, y), f)

  def get(x: Int, y: Int): T =
    get(Point(x, y))

  def get(c: Point): T =
    elements.getOrElse(c, nothing())

  def getSquaredNeighbouringCoordinates(c: Point): Iterable[Point] =
    getSquaredNeighbouringCoordinates(c._1, c._2)

  def getSquaredNeighbouringCoordinates(x: Int, y: Int): Iterable[Point] = List(
    top(x, y),
    right(x, y),
    bottom(x, y),
    left(x, y),
  )

  def getSquaredDiagonalNeighbouringCoordinates(c: Point): Iterable[Point] =
    getSquaredDiagonalNeighbouringCoordinates(c._1, c._2)

  def getSquaredDiagonalNeighbouringCoordinates(x: Int, y: Int): Iterable[Point] = List(
    topleft(x, y),
    top(x, y),
    topright(x, y),
    left(x, y),
    right(x, y),
    bottomleft(x, y),
    bottom(x, y),
    bottomright(x, y),
  )

  def getNineSquaresBasedOn(point: Point): Iterable[Point] = {
    val nb = getSquaredDiagonalNeighbouringCoordinates(point)
    nb.take(4) ++ List(point) ++ nb.drop(4)
  }

  def topleft(x: Int, y: Int): Point =
    Point(x - 1, y - 1)

  def top(x: Int, y: Int): Point =
    Point(x, y - 1)

  def topright(x: Int, y: Int): Point =
    Point(x + 1, y - 1)

  def right(x: Int, y: Int): Point =
    Point(x + 1, y)

  def bottomright(x: Int, y: Int): Point =
   Point(x + 1, y + 1)

  def bottom(x: Int, y: Int): Point =
    Point(x, y + 1)

  def left(x: Int, y: Int): Point =
    Point(x - 1, y)

  def bottomleft(x: Int, y: Int): Point =
    Point(x - 1, y + 1)
}
