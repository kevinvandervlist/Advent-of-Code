package nl.kevinvandervlist.aoc2015.day03

import scala.annotation.tailrec

object SphericalHouses {
  def one(in: List[String]): Int = in
    .map(navigate)
    .sum

  def two(in: String): Int =
    navigateWithRobot(in)

  @tailrec
  private def nav(visited: Set[(Int, Int)], pos: (Int, Int), instructions: List[Char]): Set[(Int, Int)] = instructions match {
    case Nil => visited
    case head :: tail =>
      val (x, y) = pos
      head match {
        case '^' => val newPos = x -> (y + 1); nav(visited + newPos, newPos, tail)
        case '>' => val newPos = (x + 1) -> y; nav(visited + newPos, newPos, tail)
        case 'v' => val newPos = x-> (y - 1); nav(visited + newPos, newPos, tail)
        case '<' => val newPos = (x - 1) -> y; nav(visited + newPos, newPos, tail)
      }
  }

  private def navigate(instructions: String): Int =
    nav(Set(0 -> 0), (0, 0), instructions.toCharArray.toList).size

  private def navigateWithRobot(instructions: String): Int = {
    val instr = instructions.toCharArray.toList.zipWithIndex
    val (santa, robot) = instr.partition(_._2 % 2 == 0)
    val afterSanta = nav(Set(0 -> 0), 0 -> 0, santa.map(_._1))
    val afterRobot = nav(afterSanta, 0 -> 0, robot.map(_._1))
    afterRobot.size
  }
}