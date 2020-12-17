package nl.kevinvandervlist.aoc2020.day17

import scala.collection.mutable.ListBuffer

object ConwayCube {
  def one(in: List[String]): Int =
    (0 until 6).foldLeft(parseCubes(in)) {
      case (acc, _) => acc.nextIteration()
    }.cubes

  def two(in: List[String]): Int =
    (0 until 6).foldLeft(parseCubes(in).lift) {
      case (acc, _) => acc.nextIteration()
    }.cubes

  private def parseCubes(plane: List[String]): Cubes = {
    var row = 0
    var col = 0
    val grid = new ListBuffer[ListBuffer[Char]]
    while(row < plane.length) {
      val _row = new ListBuffer[Char]
      col = 0
      while(col < plane(row).length) {
        _row.addOne(plane(row)(col))
        col += 1
      }
      grid.addOne(_row)
      row += 1
    }
    Cubes(new ListBuffer().addOne(grid))
  }
}

private case class Cubes(grid: ListBuffer[ListBuffer[ListBuffer[Char]]]) {
  private val inactive = '.'
  private val active = '#'

  def lift: HyperCubes = HyperCubes(new ListBuffer().addOne(grid))

  @inline
  def width: Int = grid.head.head.length

  @inline
  def height: Int = grid.head.length

  @inline
  def depth: Int = grid.length

  def nextIteration(): Cubes =
    grow().calculate()

  private def grow(): Cubes = {
    val emptyRow: ListBuffer[Char] = ListBuffer.fill(width)(inactive)
    val emptyColumn: ListBuffer[ListBuffer[Char]] = ListBuffer.fill(height)(emptyRow)
    Cubes(
      grid
        .prepended(emptyColumn)
        .appended(emptyColumn)
        .map(_
          .prepended(emptyRow)
          .appended(emptyRow)
          .map(_
            .prepended(inactive)
            .appended(inactive)
          )
        )
    )
  }

  private def calculate(): Cubes = {
    var x = 0
    var y = 0
    var z = 0
    val newGrid = new ListBuffer[ListBuffer[ListBuffer[Char]]]
    while(z < depth) {
      y = 0
      val zGrid = new ListBuffer[ListBuffer[Char]]
      while (y < height) {
        val row = new ListBuffer[Char]
        x = 0
        while (x < width) {
          if(isActive(x, y, z) && neighbourCountTwoOrThree(x, y, z)) {
            row.addOne(active)
          } else if(! isActive(x, y, z) && neighbourCountThree(x, y, z)) {
            row.addOne(active)
          } else {
            row.addOne(inactive)
          }
          x += 1
        }
        zGrid.addOne(row)
        y += 1
      }
      newGrid.addOne(zGrid)
      z += 1
    }

    Cubes(newGrid)
  }

  @inline
  private def neighbourCountTwoOrThree(x: Int, y: Int, z: Int): Boolean = {
    val neighbourCount = neighboursOf(x, y, z).count(_ == active)
    (neighbourCount == 2) || (neighbourCount == 3)
  }

  @inline
  private def neighbourCountThree(x: Int, y: Int, z: Int): Boolean =
    neighboursOf(x, y, z).count(_ == active) == 3

  @inline
  def positionOfM(x: Int, y: Int, z: Int): Option[Char] =
    grid.lift(z).flatMap(_.lift(y).flatMap(_.lift(x)))

  @inline
  def exists(x: Int, y: Int, z: Int): Boolean =
    positionOfM(x, y, z).isDefined

  @inline
  def isActive(x: Int, y: Int, z: Int): Boolean =
    positionOfM(x, y, z).contains(active)

  @inline
  def cubeAt(t: (Int, Int, Int)): Option[Char] =
    positionOfM(t._1, t._2, t._3)

  def neighboursOf(x: Int, y: Int, z: Int): List[Char] =
    (for {
      _x <- (x - 1) to (x + 1)
      _y <- (y - 1) to (y + 1)
      _z <- (z - 1) to (z + 1)
    } yield (_x, _y, _z))
      .iterator
      .filterNot(_ == (x, y, z))
      .flatMap(cubeAt)
      .toList

  def cubes: Int = grid
    .flatMap(_.flatMap(_.filter(_ == active)))
    .size
}

private case class HyperCubes(grid: ListBuffer[ListBuffer[ListBuffer[ListBuffer[Char]]]]) {
  private val inactive = '.'
  private val active = '#'

  @inline
  def xlen: Int = grid.head.head.head.length

  @inline
  def ylen: Int = grid.head.head.length

  @inline
  def zlen: Int = grid.head.length

  @inline
  def wlen: Int = grid.length

  def nextIteration(): HyperCubes =
    grow().calculate()

  private def grow(): HyperCubes = {
    val emptyRow: ListBuffer[Char] = ListBuffer.fill(xlen)(inactive)
    val emptyColumn: ListBuffer[ListBuffer[Char]] = ListBuffer.fill(ylen)(emptyRow)
    val emptyPlain: ListBuffer[ListBuffer[ListBuffer[Char]]] = ListBuffer.fill(zlen)(emptyColumn)
    HyperCubes(
      grid
        .prepended(emptyPlain)
        .appended(emptyPlain)
        .map(_
          .prepended(emptyColumn)
          .appended(emptyColumn)
          .map(_
            .prepended(emptyRow)
            .appended(emptyRow)
            .map(_
              .prepended(inactive)
              .appended(inactive)
            )
          )
        )
    )
  }

  private def calculate(): HyperCubes = {
    var x = 0
    var y = 0
    var z = 0
    var w = 0
    val newGrid = new ListBuffer[ListBuffer[ListBuffer[ListBuffer[Char]]]]
    while(w < wlen) {
      z = 0
      val plainGrid = new ListBuffer[ListBuffer[ListBuffer[Char]]]
      while (z < zlen) {
        y = 0
        val zGrid = new ListBuffer[ListBuffer[Char]]
        while (y < ylen) {
          val row = new ListBuffer[Char]
          x = 0
          while (x < xlen) {
            if (isActive(x, y, z, w) && neighbourCountTwoOrThree(x, y, z, w)) {
              row.addOne(active)
            } else if (!isActive(x, y, z, w) && neighbourCountThree(x, y, z, w)) {
              row.addOne(active)
            } else {
              row.addOne(inactive)
            }
            x += 1
          }
          zGrid.addOne(row)
          y += 1
        }
        plainGrid.addOne(zGrid)
        z += 1
      }
      newGrid.addOne(plainGrid)
      w += 1
    }

    HyperCubes(newGrid)
  }

  @inline
  private def neighbourCountTwoOrThree(x: Int, y: Int, z: Int, w: Int): Boolean = {
    val neighbourCount = neighboursOf(x, y, z, w).count(_ == active)
    (neighbourCount == 2) || (neighbourCount == 3)
  }

  @inline
  private def neighbourCountThree(x: Int, y: Int, z: Int, w: Int): Boolean =
    neighboursOf(x, y, z, w).count(_ == active) == 3

  @inline
  def positionOfM(x: Int, y: Int, z: Int, w: Int): Option[Char] =
    grid.lift(w).flatMap(_.lift(z).flatMap(_.lift(y).flatMap(_.lift(x))))

  @inline
  def exists(x: Int, y: Int, z: Int, w: Int): Boolean =
    positionOfM(x, y, z, w).isDefined

  @inline
  def isActive(x: Int, y: Int, z: Int, w: Int): Boolean =
    positionOfM(x, y, z, w).contains(active)

  @inline
  def cubeAt(t: (Int, Int, Int, Int)): Option[Char] =
    positionOfM(t._1, t._2, t._3, t._4)

  def neighboursOf(x: Int, y: Int, z: Int, w: Int): List[Char] =
    (for {
      _x <- (x - 1) to (x + 1)
      _y <- (y - 1) to (y + 1)
      _z <- (z - 1) to (z + 1)
      _w <- (w - 1) to (w + 1)
    } yield (_x, _y, _z, _w))
      .iterator
      .filterNot(_ == (x, y, z, w))
      .flatMap(cubeAt)
      .toList

  def cubes: Int = grid
    .flatMap(_.flatMap(_.flatMap(_.filter(_ == active))))
    .size
}