package nl.kevinvandervlist.aoc

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
}

case class RectangularGrid[T](elements: Vector[Vector[T]]) {
  def print: String =
    elements.map(_.map(_.toString).mkString("")).mkString("\n")

  def allCoordinates: Iterable[(Int, Int)] = for {
    y <- elements.indices
    x <- elements(y).indices
  } yield x -> y

  def flipTop: RectangularGrid[T] =
    copy(elements.reverse)

  def transpose: RectangularGrid[T] =
    copy(elements.transpose)

  def flipLeft: RectangularGrid[T] =
    copy(elements.transpose.reverse.transpose)

  def count(p: T => Boolean): Long =
    elements.flatten.count(p)

  def get(x: Int, y: Int): Option[T] =
    elements.lift(y).flatMap(_.lift(x))

  def set(c: (Int, Int), f: T => T): RectangularGrid[T] =
    set(c._1, c._2, f)

  def set(x: Int, y: Int, f: T => T): RectangularGrid[T] =
    copy(elements.updated(y, elements(y).updated(x, f(elements(y)(x)))))

  def get(c: (Int, Int)): Option[T] =
    get(c._1, c._2)

  def getSquaredNeighbouringCoordinates(c: (Int, Int)): Iterable[(Int, Int)] =
    getSquaredNeighbouringCoordinates(c._1, c._2)

  def getSquaredNeighbouringCoordinates(x: Int, y: Int): Iterable[(Int, Int)] = List(
    top(x, y),
    right(x, y),
    bottom(x, y),
    left(x, y),
  ).collect {
    case Some(x) => x
  }

  def getSquaredDiagonalNeighbouringCoordinates(c: (Int, Int)): Iterable[(Int, Int)] =
    getSquaredDiagonalNeighbouringCoordinates(c._1, c._2)

  def getSquaredDiagonalNeighbouringCoordinates(x: Int, y: Int): Iterable[(Int, Int)] = List(
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

  private def guardedCoordinate(predicate: => Boolean, fn: => (Int, Int)): Option[(Int, Int)] =
    if(predicate) {
      Some(fn)
    } else {
      None
    }

  def topleft(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x > 0 && y > 0, (x - 1) -> (y - 1))

  def top(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(y > 0, x -> (y - 1))

  def topright(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x < (elements.head.length - 1) && y > 0, (x + 1) -> (y - 1))

  def right(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x < (elements.head.length - 1), (x + 1) -> y)

  def bottomright(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate((x < (elements.head.length - 1)) && y < (elements.length - 1), (x + 1) -> (y + 1))

  def bottom(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(y < (elements.length - 1), x -> (y + 1))

  def left(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x > 0, (x - 1) -> y)

  def bottomleft(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate((x > 0) && (y < (elements.length - 1)), (x - 1) -> (y + 1))
}