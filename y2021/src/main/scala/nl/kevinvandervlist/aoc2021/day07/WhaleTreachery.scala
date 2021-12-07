package nl.kevinvandervlist.aoc2021.day07

import scala.annotation.tailrec

object WhaleTreachery {
  def one(line: String): Long = {
    val num = line.split(',').map(_.toInt)
    (num.min to num.max)
      .map(n => n -> calculateFuel(num, n))
      .minBy(_._2)
      ._2
  }

  def two(line: String): Long = {
    val num = line.split(',').map(_.toInt)
    val cache = (0 to (num.max - num.min)).foldLeft(Map.empty[Int, Long]){
      case (map, 0) => map + (0 -> 0)
      case (map, n) => map + (n -> (map(n - 1) + n))
    }
    (num.min to num.max)
      .map(n => n -> calculateStepTwoFuel(num, n, cache))
      .minBy(_._2)
      ._2
  }

  private def calculateFuel(ints: Array[Int], target: Int): Long = {
    var fuelCost = 0
    ints.foreach(n => fuelCost += math.abs(target - n))
    fuelCost
  }

  private def calculateStepTwoFuel(ints: Array[Int], target: Int, cache: Map[Int, Long]): Long = {
    var fuelCost = 0L
    ints.foreach(n => fuelCost += cache(math.abs(target - n)))
    fuelCost
  }
}