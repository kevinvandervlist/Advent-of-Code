package nl.kevinvandervlist.aoc

object SquareGrid {
  def apply[T](in: Iterable[Iterable[T]]): SquareGrid[T] = {
    new SquareGrid(in.map(_.toVector).toVector)
  }
  def applyInnerArray[T](in: Iterable[Array[T]]): SquareGrid[T] = {
    new SquareGrid(in.map(_.toVector).toVector)
  }
}

case class SquareGrid[T](elements: Vector[Vector[T]]) {
  def allCoordinates: Iterable[(Int, Int)] = for {
    y <- elements.indices
    x <- elements(y).indices
  } yield x -> y

  def get(x: Int, y: Int): Option[T] =
    elements.lift(y).flatMap(_.lift(x))

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

  private def guardedCoordinate(predicate: => Boolean, fn: => (Int, Int)): Option[(Int, Int)] =
    if(predicate) {
      Some(fn)
    } else {
      None
    }

  def top(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(y > 0, x -> (y - 1))

  def right(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x < (elements.head.length - 1), (x + 1) -> y)

  def bottom(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(y < (elements.length - 1), x -> (y + 1))

  def left(x: Int, y: Int): Option[(Int, Int)] =
    guardedCoordinate(x > 0, (x - 1) -> y)
}
