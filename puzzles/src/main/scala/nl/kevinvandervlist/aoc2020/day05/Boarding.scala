package nl.kevinvandervlist.aoc2020.day05

import scala.annotation.tailrec

object Boarding {
  def one(in: List[String]): Int = in
    .map(Seat.apply)
    .maxBy(_.seatId)
    .seatId

  def two(in: List[String]): Int = {
    val seats: List[Seat] = in
      .map(Seat.apply)
      .sortBy(_.seatId)

    var idx = 0
    while(idx < (seats.length - 1)) {
      if(seats(idx).seatId != (seats(idx + 1).seatId - 1)) {
        return seats(idx).seatId + 1
      }
      idx += 1
    }

    -1
  }
}

case object Seat {
  def apply(encoding: String): Seat = {
    Seat(
      row(encoding.substring(0, 7).toList),
      seat(encoding.substring(7, 10).toList),
    )
  }

  private def row(encoding: List[Char]): Int =
    bin(encoding, 'F', 0, 'B', 127)

  private def seat(encoding: List[Char]): Int =
    bin(encoding, 'L', 0, 'R', 7)

  @tailrec
  private def bin(encoding: List[Char], lowerChar: Char, lower: Int, upperChar: Char, upper: Int): Int = {
    if (encoding.isEmpty) {
      assert(lower == upper)
      lower
    } else {
      val delta = (upper - lower) / 2
      encoding.head match {
        case x if x == lowerChar => bin(encoding.tail, lowerChar, lower, upperChar, upper - delta - 1)
        case x if x == upperChar => bin(encoding.tail, lowerChar, lower + delta + 1, upperChar, upper)
      }
    }
  }
}

case class Seat(row: Int, pos: Int) {
  def seatId: Int = row * 8 + pos
}