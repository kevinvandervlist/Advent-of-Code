package nl.kevinvandervlist.aoc2016.day01

import scala.annotation.tailrec

object TaxiCab {
  def one(in: String): Int =
    traverse(North, (0 -> 0), parseInstructions(in))

  def two(in: String): Int =
    uniqueTraverse(North, (0 -> 0), parseInstructions(in), Set(0 -> 0))

  private def parseInstructions(str: String): List[Instruction] =
    str.split(Array(' ', ',')).toList.filterNot(_.isBlank).map(s => {
      s.head match {
        case 'L' => L(s.tail.toInt)
        case 'R' => R(s.tail.toInt)
      }
    })

  @tailrec
  private def traverse(heading: Heading, pos: (Int, Int), instructions: List[Instruction]): Int = instructions match {
    case Nil => pos._1.abs + pos._2.abs
    case head :: tail =>
      val nextHeading = heading.next(head)
      traverse(nextHeading, nextHeading.step(pos, head.steps), tail)
  }

  @tailrec
  private def uniqueTraverse(heading: Heading, pos: (Int, Int), instructions: List[Instruction], seen: Set[(Int, Int)]): Int = {
    instructions match {
      case Nil => pos._1.abs + pos._2.abs
      case head :: tail =>
        val nextHeading = heading.next(head)
        if(head.steps > 1) {
          val nxt = nextHeading.step(pos, 1)
          if(seen.contains(nxt)) {
            uniqueTraverse(heading, nxt, List.empty, seen ++ Set(nxt))
          } else {
            uniqueTraverse(heading, nxt, instructions.updated(0, head.minusOne), seen ++ Set(nxt))
          }
        } else {
          traverse(nextHeading, nextHeading.step(pos, head.steps), tail)
          val nxt = nextHeading.step(pos, head.steps)
          if(seen.contains(nxt)) {
            uniqueTraverse(nextHeading, nxt, List.empty, seen ++ Set(nxt))
          } else {
            uniqueTraverse(nextHeading, nxt, tail, seen ++ Set(nxt))
          }
        }
    }
  }
}

private sealed trait Instruction {
  def steps: Int
  def minusOne: Instruction
}
private case class L(steps: Int) extends Instruction {
  override def minusOne: Instruction = copy(steps = steps - 1)
}
private case class R(steps: Int) extends Instruction {
  override def minusOne: Instruction = copy(steps = steps - 1)
}

// TODO: should do this via an Enum
private sealed trait Heading {
  def next(instruction: Instruction): Heading
  def step(pos: (Int, Int), len: Int): (Int, Int) =
    steps(pos, len).last

  def steps(pos: (Int, Int), len: Int): List[(Int, Int)] = this match {
    case North => (1 to len).map(x => (pos._1, pos._2 + x)).toList
    case East => (1 to len).map(x => (pos._1 + x, pos._2)).toList
    case South => (1 to len).map(x => (pos._1, pos._2 - x)).toList
    case West => (1 to len).map(x => (pos._1 - x, pos._2)) .toList
  }
}

private case object North extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(_) => West
    case R(_) => East
  }
}
private case object East extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(_) => North
    case R(_) => South
  }
}

private case object South extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(_) => East
    case R(_) => West
  }
}

private case object West extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(_) => South
    case R(_) => North
  }
}