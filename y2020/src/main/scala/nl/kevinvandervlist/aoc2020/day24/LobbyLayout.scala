package nl.kevinvandervlist.aoc2020.day24

import scala.annotation.tailrec

object X {
  def one(in: List[String]): Int =
    seed(in)
     .values
      .count(identity)

  def two(in: List[String]): Int = (1 to 100)
    .foldLeft(infiniteNeighbours(seed(in))) {
      case (floor, _) => nextDay(floor)
    }.values.count(identity)

  // black is true
  private def seed(in: List[String]): Map[(Int, Int), Boolean] = in
    .map(tileToCoords)
    .foldLeft(Map.empty[(Int, Int), Boolean]) {
      case (acc, t) if acc.contains(t) => acc + (t -> ! acc(t))
      case (acc, t) => acc + (t -> true)
    }

  private def tileToCoords(t: String): (Int, Int) = {
    // neighbours are a trapezoidal coordinate system:
    // Joshua Kidd (https://math.stackexchange.com/users/441008/joshua-kidd),
    // Hexagon grid coordinate system, URL (version: 2018-02-09): https://math.stackexchange.com/q/2643016
    @tailrec
    def rec(instr: List[Char], x: Int, y: Int): (Int, Int) = instr match {
      case Nil => x -> y
      case 'e' :: tail => rec(tail, x + 1, y)
      case 's' :: 'e' :: tail => rec(tail, x, y + 1)
      case 's' :: 'w' :: tail => rec(tail, x - 1, y + 1)
      case 'w' :: tail => rec(tail, x - 1, y)
      case 'n' :: 'w' :: tail => rec(tail, x, y - 1)
      case 'n' :: 'e' :: tail => rec(tail, x + 1, y - 1)
    }
    rec(t.toList, 0, 0)
  }

  private def nextDay(floor: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
    val updatedFloor = floor.map {
      case (pos, isBlack) if isBlack && (blackNeighbours(floor, pos) == 0 || blackNeighbours(floor, pos) > 2) => pos -> false
      case (pos, isBlack) if ! isBlack && blackNeighbours(floor, pos) == 2 => pos -> true
      case (pos, isBlack) => pos -> isBlack
    }
    infiniteNeighbours(updatedFloor)
  }

  private def neighbours(pos: (Int, Int)): List[(Int, Int)] = pos match {
    case (x, y) => List(
      (x + 1) -> y,
      x -> (y + 1),
      (x - 1) -> (y + 1),
      (x - 1) -> y,
      x -> (y - 1),
      (x + 1) -> (y - 1)
    )
  }

  private def blackNeighbours(floor: Map[(Int, Int), Boolean], pos: (Int, Int)): Int = {
    neighbours(pos)
      .map(floor.getOrElse(_, false))
      .count(identity)
  }

  // a bit hacky, just eagerly fill the whole map with white tiles...
  private def infiniteNeighbours(floor: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
    val newNeighbours = floor
      .keys
      .flatMap(neighbours)
      .filterNot(floor.contains)
    floor ++ newNeighbours.map(c => c -> false)
  }
}