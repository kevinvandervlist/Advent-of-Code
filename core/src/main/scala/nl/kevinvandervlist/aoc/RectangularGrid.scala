package nl.kevinvandervlist.aoc

import nl.kevinvandervlist.aoc.Point
import scala.collection.mutable

object RectangularGrid {
  def apply[T](in: Iterable[Iterable[T]]): RectangularGrid[T] = {
    new RectangularGrid(in.map(_.toVector).toVector)
  }
  def applyInnerArray[T](in: Iterable[Array[T]]): RectangularGrid[T] = {
    new RectangularGrid(in.map(_.toVector).toVector)
  }
  def apply[T](xSize: Int, ySize: Int, default: T): RectangularGrid[T] = {
    new RectangularGrid(Vector.fill(ySize)(Vector.fill(xSize)(default)))
  }
  def fromSingleDigitGrid(in: List[String]): RectangularGrid[Int] =
    RectangularGrid.applyInnerArray(
      in.map(_.toCharArray.map(_.toString.toInt))
    )
  def fromSingleCharGrid(in: List[String]): RectangularGrid[Char] =
    RectangularGrid.applyInnerArray(in.map(_.toCharArray))
}

case class RectangularGrid[T](elements: Vector[Vector[T]]) {
  def print: String =
    elements.map(_.map(_.toString).mkString("")).mkString("\n")

  def width: Int =
    elements.head.length

  def height: Int =
    elements.length

  def allCoordinates: Iterable[Point] = for {
    y <- elements.indices
    x <- elements(y).indices
  } yield Point(x, y)

  def flipTop: RectangularGrid[T] =
    copy(elements.reverse)

  def transpose: RectangularGrid[T] =
    copy(elements.transpose)

  def flipLeft: RectangularGrid[T] =
    copy(elements.transpose.reverse.transpose)

  def count(p: T => Boolean): Long =
    elements.flatten.count(p)

  def set(c: Point, f: T => T): RectangularGrid[T] =
    set(c._1, c._2, f)

  def set(x: Int, y: Int, f: T => T): RectangularGrid[T] =
    copy(elements.updated(y, elements(y).updated(x, f(elements(y)(x)))))

  def get(x: Int, y: Int): Option[T] =
    elements.lift(y).flatMap(_.lift(x))
      
  def get(c: Point): Option[T] =
    get(c._1, c._2)

  def getSquaredNeighbouringCoordinates(c: Point): Iterable[Point] =
    getSquaredNeighbouringCoordinates(c._1, c._2)

  def getSquaredNeighbouringCoordinates(x: Int, y: Int): Iterable[Point] = List(
    top(x, y),
    right(x, y),
    bottom(x, y),
    left(x, y),
  ).collect {
    case Some(x) => x
  }

  def getSquaredDiagonalNeighbouringCoordinates(c: Point): Iterable[Point] =
    getSquaredDiagonalNeighbouringCoordinates(c._1, c._2)

  def getSquaredDiagonalNeighbouringCoordinates(x: Int, y: Int): Iterable[Point] = List(
    topleft(x, y),
    top(x, y),
    topright(x, y),
    right(x, y),
    bottomright(x, y),
    bottom(x, y),
    bottomleft(x, y),
    left(x, y),
  ).collect {
    case Some(x) => x
  }

  private def guardedCoordinate(predicate: => Boolean, fn: => Point): Option[Point] =
    if(predicate) {
      Some(fn)
    } else {
      None
    }

  def topleft(x: Int, y: Int): Option[Point] =
    guardedCoordinate(x > 0 && y > 0, Point(x - 1, y - 1))

  def top(x: Int, y: Int): Option[Point] =
    guardedCoordinate(y > 0, Point(x, y - 1))

  def topright(x: Int, y: Int): Option[Point] =
    guardedCoordinate(x < (width - 1) && y > 0, Point(x + 1, y - 1))

  def right(x: Int, y: Int): Option[Point] =
    guardedCoordinate(x < (width - 1), Point(x + 1, y))

  def bottomright(x: Int, y: Int): Option[Point] =
    guardedCoordinate((x < (width - 1)) && y < (height - 1), Point(x + 1, y + 1))

  def bottom(x: Int, y: Int): Option[Point] =
    guardedCoordinate(y < (height - 1), Point(x, y + 1))

  def left(x: Int, y: Int): Option[Point] =
    guardedCoordinate(x > 0, Point(x - 1, y))

  def bottomleft(x: Int, y: Int): Option[Point] =
    guardedCoordinate((x > 0) && (y < (height - 1)), Point(x - 1, y + 1))

  // Thanks wikipedia
  def aStar(start: Point, target: Point, heuristic: Point => Int = p => 1, weight: T => Int): List[Point] = {
    var cameFrom = mutable.Map.empty[Point, Point]
    var gScore = mutable.Map(start -> 0) // cost of cheapest path from start to currently known
    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how short a path from start to finish can be if it goes through n.
    var fScore = mutable.Map(start -> heuristic(start))

    val pq = mutable.PriorityQueue.empty[Point](
      Ordering.by(fScore.getOrElse(_, Int.MaxValue)).reverse
    )
    pq.addOne(start)

    while(pq.nonEmpty) {
      var current = pq.dequeue()

      // have we found a path?
      if(current == target) {
        var path = List(current)
        while(cameFrom.contains(current)) {
          current = cameFrom(current)
          path = current :: path
        }
        return path
      }

      // Otherwise continue exploration
      getSquaredNeighbouringCoordinates(current) foreach { nb =>
        val tentative = gScore.get(current)
          .flatMap(g => get(current).map(c => weight(c) + g))
          .getOrElse(Int.MaxValue)
        if(tentative < gScore.getOrElse(nb, Int.MaxValue)) {
          // improvement found
          cameFrom.addOne(nb, current)
          gScore.addOne(nb, tentative)
          fScore.addOne(nb, tentative + heuristic(nb))
          if(! pq.exists(_ == nb)) {
            pq.addOne(nb)
          }
        }
      }
    }

    List.empty
  }
}
