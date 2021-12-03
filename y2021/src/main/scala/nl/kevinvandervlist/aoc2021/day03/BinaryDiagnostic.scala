package nl.kevinvandervlist.aoc2021.day03

import scala.annotation.tailrec

object BinaryDiagnostic {
  def one(in: List[String]): Int =
    gamma(in) * epsilon(in)

  def two(in: List[String]): Int =
    oxygen(in) * scrubber(in)

  private def gamma(readings: List[String]): Int = {
    val len = readings.head.length
    val binary = (0 until readings.head.length)
      .map(mostCommonAt(_, readings))
      .mkString("")
    Integer.parseInt(binary, 2)
  }

  private def epsilon(readings: List[String]): Int = {
    val len = readings.head.length
    val binary = (0 until readings.head.length)
      .map(mostCommonAt(_, readings))
      .map {
        case "1" => "0"
        case "0" => 1
      }
      .mkString("")
    Integer.parseInt(binary, 2)
  }

  private def oxygen(readings: List[String]): Int =
    Integer.parseInt(filterCriteria(readings, '1'), 2)


  private def scrubber(readings: List[String]): Int =
    Integer.parseInt(filterCriteria(readings, '0'), 2)

  private def filterCriteria(in: List[String], keepWhenEqual: Char): String = {
    rec(in, 0, keepWhenEqual).head
  }

  @tailrec
  private def rec(in: List[String], pos: Int, keepWhenEqual: Char): List[String] = in match {
    case element :: Nil => List(element)
    case _ =>
      val ones = in.filter(s => s.charAt(pos) == '1').size
      val zeroes = in.filter(s => s.charAt(pos) == '0').size
      if(ones == zeroes) {
        rec(keep(in, keepWhenEqual, pos), pos + 1, keepWhenEqual)
      } else {
        if(keepWhenEqual == '1') {
          if(ones > zeroes) {
            // keep with one at pos
            rec(keep(in, '1', pos), pos + 1, keepWhenEqual)
          } else {
            // keep with zero at pos
            rec(keep(in, '0', pos), pos + 1, keepWhenEqual)
          }
        } else {
          if(ones < zeroes) {
            // keep with one at pos
            rec(keep(in, '1', pos), pos + 1, keepWhenEqual)
          } else {
            // keep with zero at pos
            rec(keep(in, '0', pos), pos + 1, keepWhenEqual)
          }
        }
      }
  }

  private def keep(lst: List[String], what: Char, at: Int): List[String] = lst.filter {
    case s => s.charAt(at) == what
  }

  private def mostCommonAt(pos: Int, readings: List[String]): String = {
    val all = readings.map(_.charAt(pos))
    val ones = all.filter(_ == '1')
    val zeroes = all.filter(_ == '0')
    if(ones.size > zeroes.size) {
      return "1"
    } else {
      return "0"
    }
  }
}
