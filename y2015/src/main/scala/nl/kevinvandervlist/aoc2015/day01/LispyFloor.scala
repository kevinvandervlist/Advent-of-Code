package nl.kevinvandervlist.aoc2015.day01

import scala.annotation.tailrec

object LispyFloor {
  def one(in: String): Int =
    interpret(0, in.toCharArray.toList)

  def two(in: String): Int =
    arriveAtBasement(0, 0, in.toCharArray.toList)

  @tailrec
  private def interpret(cur: Int, stack: List[Char]): Int = stack match {
    case Nil => cur
    case head :: tail => head match {
      case '(' =>interpret(cur + 1, tail)
      case ')' => interpret(cur - 1, tail)
    }
  }

  @tailrec
  private def arriveAtBasement(step: Int, cur: Int, stack: List[Char]): Int = {
    if(cur == -1) {
      return step
    }
    stack match {
      case Nil => -1
      case head :: tail => head match {
        case '(' => arriveAtBasement(step + 1, cur + 1, tail)
        case ')' => arriveAtBasement(step + 1, cur - 1, tail)
      }
    }
  }
}
