package nl.kevinvandervlist.aoc2015.day06

import scala.collection.mutable.ListBuffer

object FireHazard {
  def one(in: List[String]): Int = {
    val grid: Array[Array[Boolean]] = Array.fill(1000)(Array.fill(1000)(false))
    for(i <- parse(in)) {
      for ((x, y) <- i.coordinates) {
        i match {
          case _: On => grid(x)(y) = true
          case _: Off => grid(x)(y) = false
          case _: Toggle => grid(x)(y) = ! grid(x)(y)
        }
      }
    }
    grid.flatMap(_.filter(_ == true)).length
  }
  def two(in: List[String]): Int = {
    val grid: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))
    for(i <- parse(in)) {
      for ((x, y) <- i.coordinates) {
        i match {
          case _: On => grid(x)(y) = grid(x)(y) + 1
          case _: Off if grid(x)(y) > 0 => grid(x)(y) = grid(x)(y) - 1
          case _: Off => // noop
          case _: Toggle => grid(x)(y) = grid(x)(y) + 2
        }
      }
    }
    grid.map(_.sum).sum

  }

  private def parse(in: List[String]): List[Instruction] = {
    val instructions = new ListBuffer[Instruction]
    val on = "turn on (\\d+),(\\d+) through (\\d+),(\\d+)".r
    val toggle = "toggle (\\d+),(\\d+) through (\\d+),(\\d+)".r
    val off = "turn off (\\d+),(\\d+) through (\\d+),(\\d+)".r
    for(i <- in) {
      if(on.matches(i)) {
        val on(a1, a2, b1, b2) = i
        instructions.addOne(On(a1.toInt -> a2.toInt, b1.toInt -> b2.toInt))
      } else if(toggle.matches(i)) {
        val toggle(a1, a2, b1, b2) = i
        instructions.addOne(Toggle(a1.toInt -> a2.toInt, b1.toInt -> b2.toInt))
      } else if(off.matches(i)) {
        val off(a1, a2, b1, b2) = i
        instructions.addOne(Off(a1.toInt -> a2.toInt, b1.toInt -> b2.toInt))
      } else {
        ???
      }
    }
    instructions.toList
  }
}

private sealed trait Instruction {
  def a: (Int, Int)
  def b: (Int, Int)
  def coordinates: List[(Int, Int)] = {
    assert(a._1 <= b._1, s"A: $a, B: $b")
    assert(a._2 <= b._2, s"A: $a, B: $b")

    val xRange = (a._1 to b._1).toList
    val yRange = (a._2 to b._2).toList

    for {
      x <- xRange
      y <- yRange
    } yield (x, y)
  }
}
private case class On(a: (Int, Int), b: (Int, Int)) extends Instruction
private case class Toggle(a: (Int, Int), b: (Int, Int)) extends Instruction
private case class Off(a: (Int, Int), b: (Int, Int)) extends Instruction