package nl.kevinvandervlist.aoc2021.day04

object GiantSquid {
  def one(in: List[String]): Int = {
    val draws = in.head.split(",").map(_.toInt)
    // double tail to drop the empty line between the draws and the first board,
    // appended empty string to ensure final board creation
    var boards = getBoards(in.tail.tail.appended(""))
    for(d <- draws) {
      boards = boards.map(_.mark(d))
      val winners = boards.filter(_.hasWon)
      if(winners.nonEmpty) {
        return winners.map(_.boardValue(d)).max
      }
    }
    return -1
  }

  def two(in: List[String]): Int = {
    val draws = in.head.split(",").map(_.toInt)
    // double tail to drop the empty line between the draws and the first board,
    // appended empty string to ensure final board creation
    var boards = getBoards(in.tail.tail.appended(""))
    for(d <- draws) {
      boards = boards.map(_.mark(d))
      val notWonYet = boards.filterNot(_.hasWon)
      if(notWonYet.isEmpty) {
        return boards.map(_.boardValue(d)).min
      } else {
        boards = notWonYet
      }
    }
    return -1
  }

  private def getBoards(raw: List[String]): List[Board] = {
    var allBoards = List.empty[Board]
    var currentBoard = List.empty[String]
    raw.foreach(row => {
      if(row.isEmpty) {
        allBoards = allBoards :+ buildBoard(currentBoard)
        currentBoard = List.empty
      } else {
        currentBoard = currentBoard :+ row
      }
    })
    return allBoards
  }

  private def buildBoard(rows: List[String]): Board = {
    val numbers = rows
      .map(_.split(' '))
      .map(_.toList
        .filterNot(_.isEmpty)
        .map(_.toInt)
      )
    Board(numbers)
  }
}

private case class Board(val grid: List[List[Int]], markedGrid: List[List[Boolean]] = List.fill(5)(List.fill(5)(false))) {
  def has(number: Int): Boolean =
    grid.flatten.contains(number)

  def mark(number: Int): Board = {
    for (ri <- 0 until grid.length) {
      for (rc <- 0 until grid(ri).length)
        if (grid(ri)(rc) == number) {
          return Board(
            grid,
            markedGrid.updated(ri, markedGrid(ri).updated(rc, true))
          )
        }
    }
    this
  }

  def hasWon: Boolean = {
    markedGrid.filter(row => row.forall(e => e)).nonEmpty ||
      markedGrid.transpose.filter(row => row.forall(e => e)).nonEmpty
  }

  def unMarked: List[Int] = {
    var lst = List.empty[Int]
    for (ri <- 0 until grid.length) {
      for (rc <- 0 until grid(ri).length)
        if (! markedGrid(ri)(rc)) {
          lst = lst :+ grid(ri)(rc)
        }
    }
    lst
  }

  def boardValue(lastDraw: Int): Int = {
    unMarked.sum * lastDraw
  }
}