package nl.kevinvandervlist.aoc2020.day10

import scala.annotation.tailrec

object BatteryArray {
  def one(in: List[String]): Int = {
    val sorted = in.map(_.toInt).sorted
    val tested = deltaJolts(sorted.appended(sorted.last + 3))
    tested(1) * tested(3)
  }

  def two(in: List[String]): Long = {
    val sorted = in.map(_.toInt).sorted
    val includingCharger = sorted.prepended(0).appended(sorted.last + 3)
    var partials: Map[Int, Long] = Map(0 -> 1)

    var idx = 1
    while(idx < includingCharger.length) {
      val c = includingCharger(idx)
      (c - 3).until(c).foreach(p => {
        val v = partials.getOrElse(c, 0L) + partials.getOrElse(p, 0L)
        partials = partials + (c -> v)
      })
      idx += 1
    }

    partials(includingCharger.last)
  }

  private def deltaJolts(in: List[Int]): Map[Int, Int] = {
    @tailrec
    def rec(cur: Int, state: Map[Int, Int], rest: List[Int]): Map[Int, Int] = rest match {
      case Nil => state
      case head :: tail =>
        val delta = head - cur
        rec(head, state + (delta -> (state.getOrElse(delta, 0) + 1)), tail)
    }
    rec(0, Map.empty, in)
  }
}