package nl.kevinvandervlist.aoc2021.day18

import scala.annotation.tailrec

object Snailfish {
  def one(in: List[String]): Int = {
    val nums = in.map(parse)
    val answer = nums.tail.foldLeft(nums.head) {
      case (sum, next) => reduce(add(sum, next))
    }
    reduceMagnitude(answer) match {
      case Number(answer) => answer
      case _ => ???
    }
  }

  def two(in: List[String]): Int = {
    val nums = in.map(parse)
    val pairs = nums.combinations(2).map(p => p.head -> p.tail.head).toList
    (pairs ++ pairs.map(_.swap)).map {
      case (a, b) => reduceMagnitude(reduce(add(a, b)))
    }.collect {
      case Number(mag) => mag
    }.max
  }

  private def print(in: List[SnailToken]): String = {
    in.map {
      case Open => "["
      case Close => "]"
      case Comma => ","
      case Number(x) => x.toString
    }.mkString("")
  }

  private def parse(line: String): List[SnailToken] = line.map {
    case '[' => Open
    case ']' => Close
    case ',' => Comma
    case n => Number(n.toString.toInt)
  }.toList

  private def add(one: List[SnailToken], two: List[SnailToken]): List[SnailToken] =
    List(Open) ++ one ++ List(Comma) ++ two ++ List(Close)

  private def reduce(tokens: List[SnailToken]): List[SnailToken] = {
    var current = List.empty[SnailToken]
    var previous = tokens
    while(true) {
      current = explode(previous)
      if(current == previous) {
        current = split(previous)
      }
      // if neither changed our result we can stop reducing
      if(current == previous) {
        return current
      }
      previous = current
    }
    ???
  }

  private def explode(num: List[SnailToken]): List[SnailToken] = {
    // we are looking for a [int, int] pattern at depth 5
    var depth = 0
    var idx = 0
    for(idx <- num.indices) {
      num(idx) match {
        case Open => depth += 1
        case Close => depth -= 1
        case Comma =>
        case Number(n) =>
      }
      if(depth >= 5) {
        if((num.length - 1) < idx + 3) {
          return num
        }
        (num(idx + 1), num(idx + 3)) match {
          case (lhs: Number, rhs: Number) =>
            // we got the pattern
            // first neighbour numbers (if any)
            val firstLeftNumber = num.indices.take(idx).reverse.map(n => n -> num(n)).collectFirst {
              case (n, Number(number)) => n -> Number(number)
            }
            val firstRightNumber = num.indices.drop(idx + 5).map(n => n -> num(n)).collectFirst {
              case (n, Number(number)) => n -> Number(number)
            }
            // update the sequence with the exploded state
            var explodedList = num
            firstLeftNumber.foreach {
              case (n, v) => explodedList =
                explodedList.updated(n, v.copy(v.number + lhs.number))
            }
            firstRightNumber.foreach {
              case (n, v) => explodedList =
                explodedList.updated(n, v.copy(v.number + rhs.number))
            }
            explodedList = explodedList.take(idx) ++ List(Number(0)) ++ explodedList.drop(idx + 5)
            return explodedList
          case _ =>
        }
      }
    }
    num
  }

  private def split(num: List[SnailToken]): List[SnailToken] = {
    // we are looking for an int > 10
    var idx = 0
    for(idx <- num.indices) {
      num(idx) match {
        case Number(x) if x > 9 =>
          val splitted = List(Open, Number(x / 2), Comma, Number(x - (x / 2)), Close)
          return num.take(idx) ++ splitted ++ num.drop(idx + 1)
        case _ =>
      }
    }
    num
  }

  @tailrec
  def reduceMagnitude(tokens: List[SnailToken]): SnailToken = tokens match {
    case v :: Nil => v
    case otherwise => reduceMagnitude(magnitude(otherwise))
  }

  private def magnitude(num: List[SnailToken]): List[SnailToken] = {
    // we are looking for a [int, int] pattern at depth 5
    var depth = 0
    var idx = 0
    for(idx <- num.indices) {
      if((num.length - 1) < idx + 3) {
        return num
      }
      (num(idx + 1), num(idx + 3)) match {
        case (lhs: Number, rhs: Number) =>
          // we got the pattern
          // update the sequence with the exploded state
          var explodedList = num
          explodedList = explodedList.take(idx) ++ List(Number(lhs.number * 3 + rhs.number * 2)) ++ explodedList.drop(idx + 5)
          return explodedList
        case _ =>
      }
    }
    num
  }
}

sealed trait SnailToken
case object Open extends SnailToken
case object Close extends SnailToken
case object Comma extends SnailToken
case class Number(number: Int) extends SnailToken