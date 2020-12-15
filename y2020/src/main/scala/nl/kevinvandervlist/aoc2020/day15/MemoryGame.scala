package nl.kevinvandervlist.aoc2020.day15

object MemoryGame {
  def one(in: String): Int =
    compute(in.trim().split(',').map(_.toInt), 2020)

  def two(in: String): Int =
    compute(in.trim().split(',').map(_.toInt), 30_000_000)

  private def compute(init: Array[Int], roundLimit: Int): Int = {
    // Arrays are initialized with 0 by spec
    val cur: Array[Int] = new Array[Int](roundLimit)
    val prev: Array[Int] = new Array[Int](roundLimit)
    var round: Int = 1
    var lastNumberSpoken: Int = 0
    // setup
    while(round <= init.length) {
      lastNumberSpoken = init(round - 1)
      cur(lastNumberSpoken) = round
      round += 1
    }
    while(round <= roundLimit) {
      if(prev(lastNumberSpoken) > 0) {
        // seen before
        lastNumberSpoken = cur(lastNumberSpoken) - prev(lastNumberSpoken)
      } else {
        // new
        lastNumberSpoken = 0
      }

      // We update regardless of presence of lastNumberSpoken in prev, worst case it remains 0
      prev(lastNumberSpoken) = cur(lastNumberSpoken)
      cur(lastNumberSpoken) = round

      round += 1
    }
    lastNumberSpoken
  }
}
