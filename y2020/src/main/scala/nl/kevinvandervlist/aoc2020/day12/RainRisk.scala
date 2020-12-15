package nl.kevinvandervlist.aoc2020.day12

import scala.annotation.tailrec

object RainRisk {
  def one(in: List[String]): Int =
    traverse(East, (0 -> 0), parseInstructions(in))

  def two(in: List[String]): Int =
    traverseWaypoint(East, (0 -> 0), (10 -> 1), parseInstructions(in))

  private def parseInstructions(in: List[String]): List[Instruction] = in
    .map(s => {
      s.head match {
        case 'L' => assert(isSquareAngleOnly(s.tail.toInt)); L(s.tail.toInt)
        // Only do a left rotation translation for convenience
        case 'R' => assert(isSquareAngleOnly(s.tail.toInt)); L(360 - s.tail.toInt)
        case 'N' => N(s.tail.toInt)
        case 'S' => S(s.tail.toInt)
        case 'E' => E(s.tail.toInt)
        case 'W' => W(s.tail.toInt)
        case 'F' => F(s.tail.toInt)
      }
    })

  private def isSquareAngleOnly(n: Int): Boolean = n match {
    case 90 => true
    case 180 => true
    case 270 => true
    case _ => false
  }

  @tailrec
  private def traverse(heading: Heading, boatPos: (Int, Int), instructions: List[Instruction]): Int = instructions match {
    case Nil => boatPos._1.abs + boatPos._2.abs
    case (head: H) :: tail =>
      val nextHeading = heading.next(head)
      traverse(nextHeading, boatPos, tail)
    case head :: tail =>
      val nextHeading = heading.next(head)
      traverse(heading, nextHeading.step(boatPos, head.stepSize), tail)
  }

  @tailrec
  private def traverseWaypoint(heading: Heading, boatPos: (Int, Int), wayPoint: (Int, Int), instructions: List[Instruction]): Int = instructions match {
    case Nil => boatPos._1.abs + boatPos._2.abs
    case (head: F) :: tail =>
      // Waypoint is relative to the boat!
      val updatedPos = (
        boatPos._1 + (wayPoint._1 * head.stepSize),
        boatPos._2 + (wayPoint._2 * head.stepSize),
      )
      traverseWaypoint(heading, updatedPos, wayPoint, tail)
    case (head: H) :: tail =>
      // https://en.wikipedia.org/wiki/Rotation_matrix#Common_rotations
      val matrix = rotationMatrixFor(head)
      val nextHeading = heading.next(head)
      val updatedWaypoint = (
        matrix(0)(0) * wayPoint._1 + matrix(0)(1) * wayPoint._2,
        matrix(1)(0) * wayPoint._1 + matrix(1)(1) * wayPoint._2
      )
      traverseWaypoint(nextHeading, boatPos, updatedWaypoint, tail)
    case head :: tail =>
      val nextHeading = heading.next(head)
      val updatedWaypoint = nextHeading.step(wayPoint, head.stepSize)
      traverseWaypoint(heading, boatPos, updatedWaypoint, tail)
  }

  private def rotationMatrixFor(head: H): List[List[Int]] = (head: @unchecked) match {
    case L(90) => List(
      List(0, -1),
      List(1, 0)
    )
    case L(180) => List(
      List(-1, 0),
      List(0, -1)
    )
    case L(270) => List(
      List(0, 1),
      List(-1, 0)
    )
  }
}

private sealed trait Instruction {
  def stepSize: Int
}
private sealed trait H extends Instruction {
  def deg: Int
}
private case class L(deg: Int) extends H {
  def stepSize: Int = 0
}

private case class N(stepSize: Int) extends Instruction
private case class S(stepSize: Int) extends Instruction
private case class E(stepSize: Int) extends Instruction
private case class W(stepSize: Int) extends Instruction
private case class F(stepSize: Int) extends Instruction

// TODO: should do this via an Enum or something
private sealed trait Heading {
  def next(instruction: Instruction): Heading
  def step(pos: (Int, Int), len: Int): (Int, Int) = this match {
    case North => pos._1 -> (pos._2 + len)
    case East => (pos._1 + len) -> pos._2
    case South => pos._1 -> (pos._2 - len)
    case West => (pos._1 - len) -> pos._2
  }
}

private case object North extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(90) => West
    case L(180) => South
    case L(270) => East
    case N(_) => North
    case E(_) => East
    case S(_) => South
    case W(_) => West
    case F(_) => this
  }
}
private case object East extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(90) => North
    case L(180) => West
    case L(270) => South
    case N(_) => North
    case E(_) => East
    case S(_) => South
    case W(_) => West
    case F(_) => this
  }
}

private case object South extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(90) => East
    case L(180) => North
    case L(270) => West
    case N(_) => North
    case E(_) => East
    case S(_) => South
    case W(_) => West
    case F(_) => this
  }
}

private case object West extends Heading {
  override def next(instruction: Instruction): Heading = instruction match {
    case L(90) => South
    case L(180) => East
    case L(270) => North
    case N(_) => North
    case E(_) => East
    case S(_) => South
    case W(_) => West
    case F(_) => this
  }
}