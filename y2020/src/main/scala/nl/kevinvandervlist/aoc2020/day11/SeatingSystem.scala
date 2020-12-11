package nl.kevinvandervlist.aoc2020.day11

import scala.collection.mutable.ListBuffer

object SeatingSystem {
  def one(in: List[String]): Int = {
    var seating = parseSeating(in)
    var nextSeating = seating.nextIteration
    while(seating != nextSeating) {
      seating = nextSeating
      nextSeating = nextSeating.nextIteration
    }
    nextSeating.occupiedSeats
  }

  def two(in: List[String]): Int = {
    var seating = parseSeating(in)
    var nextSeating = seating.nextIterationPartTwo
    while(seating != nextSeating) {
      seating = nextSeating
      nextSeating = nextSeating.nextIterationPartTwo
    }
    nextSeating.occupiedSeats
  }

  private def parseSeating(value: List[String]): Seating = {
    var row = 0
    var col = 0
    val grid = new ListBuffer[ListBuffer[Char]]
    while(row < value.length) {
      val _row = new ListBuffer[Char]
      col = 0
      while(col < value(row).length) {
        _row.addOne(value(row)(col))
        col += 1
      }
      grid.addOne(_row)
      row += 1
    }
    Seating(grid.map(_.toList).toList)
  }
}

private case class Seating(grid: List[List[Char]]) {
  private val floor = '.'
  private val empty = 'L'
  private val occupied = '#'

  def width: Int = grid.head.length
  def height: Int = grid.length

  def nextIteration: Seating = nextIteration(4, true)
  def nextIterationPartTwo: Seating = nextIteration(5, false)

  private def nextIteration(minNeighbours: Int, directNeighboursOnly: Boolean): Seating = {
    var x = 0
    var y = 0
    val newGrid = new ListBuffer[ListBuffer[Char]]
    while(y < height) {
      val row = new ListBuffer[Char]
      x = 0
      while(x < width) {
        val neighbourCount = neighboursOf(x, y, directNeighboursOnly).count(_ == occupied)
        if(positionOf(x, y) == empty && neighbourCount == 0) {
          row.addOne(occupied)
        } else if(positionOf(x, y) == occupied && neighbourCount >= minNeighbours) {
          row.addOne(empty)
        } else {
          row.addOne(positionOf(x, y))
        }
        x += 1
      }
      newGrid.addOne(row)
      y += 1
    }
    Seating(newGrid.map(_.toList).toList)
  }

  def positionOf(x: Int, y: Int): Char = grid(y)(x)

  def inLineOf(_x: Int, fx: Int => Int, _y: Int, fy: Int => Int, once: Boolean): Option[Char] = {
    var stillAValidPosition = true
    var x = fx(_x)
    var y = fy(_y)
    if(once) {
      return grid.lift(y).flatMap(_.lift(x))
    }
    while(stillAValidPosition) {
      grid.lift(y).flatMap(_.lift(x)) match {
        case None => stillAValidPosition = false
        case Some(result) =>
          if(result != floor) {
            return Some(result)
          }
          x = fx(x)
          y = fy(y)
      }
    }
    None
  }

  def neighboursOf(x: Int, y: Int, direct: Boolean): List[Char] = {
    val neighbours = new ListBuffer[Char]
    // top left diagonal
    if(x > 0 && y > 0) {
      inLineOf(x, x => x - 1, y, y => y - 1, direct)
        .foreach(neighbours.addOne)
    }
    // top mid
    if(y > 0) {
      inLineOf(x, identity, y, y => y - 1, direct)
        .foreach(neighbours.addOne)
    }
    // top right
    if(x < (width - 1) && y > 0) {
      inLineOf(x, x => x + 1, y, y => y - 1, direct)
        .foreach(neighbours.addOne)
    }

    // mid left
    if(x > 0) {
      inLineOf(x, x => x - 1, y, identity, direct)
        .foreach(neighbours.addOne)
    }
    // mid right
    if(x < (width - 1)) {
      inLineOf(x, x => x + 1, y, identity, direct)
        .foreach(neighbours.addOne)
    }

    // bottom left
    if(x > 0 && y < (height - 1)) {
      inLineOf(x, x => x - 1, y, y => y + 1, direct)
        .foreach(neighbours.addOne)
    }
    // bottom mid
    if(y < (height - 1)) {
      inLineOf(x, identity, y, y => y + 1, direct)
        .foreach(neighbours.addOne)
    }
    // bottom right
    if(x < (width - 1) && y < (height - 1)) {
      inLineOf(x, x => x + 1, y, y => y + 1, direct)
        .foreach(neighbours.addOne)
    }
    neighbours.toList
  }

  def occupiedSeats: Int = grid
    .flatMap(_.filter(_ == occupied))
    .size

  override def toString: String =
    grid.map(_.mkString("")).mkString("\n")
}