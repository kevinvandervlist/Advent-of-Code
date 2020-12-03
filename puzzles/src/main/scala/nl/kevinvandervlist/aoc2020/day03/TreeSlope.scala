package nl.kevinvandervlist.aoc2020.day03

import scala.collection.mutable.ListBuffer

object TreeSlope {

  def countTrees2(in: List[String]): List[BigInt] = {
    val strategies = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    )
    strategies.map(evaluate(slope(in), _))
  }

  def countTrees(in: List[String]): BigInt =
    evaluate(slope(in), (3, 1))

  private def slope(in: List[String]): Slope = Slope(in.map(_.map {
    case '.' => Open
    case '#' => Tree
  }).toIndexedSeq)

  private def evaluate(slope: Slope, strat: (Int, Int)): BigInt =
    slope.travel(strat._1, strat._2).count(_ == Tree)
}

private case class Slope(area: IndexedSeq[IndexedSeq[Plot]]) {
  private val len = area.head.length
  def travel(xOffset: Int, yOffset: Int): IndexedSeq[Plot] = {
    var row = 0
    var col = 0
    val results = new ListBuffer[Plot]
    while(row < area.length) {
      results.addOne(area(row)(col % len))
      row += yOffset
      col += xOffset
    }
    results.toIndexedSeq
  }
}

private sealed trait Plot
private case object Tree extends Plot
private case object Open extends Plot