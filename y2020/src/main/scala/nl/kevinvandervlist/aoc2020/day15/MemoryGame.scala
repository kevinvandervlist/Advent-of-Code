package nl.kevinvandervlist.aoc2020.day15

object MemoryGame {
  def one(in: String): Int =
    compute(in.trim().split(',').map(_.toInt), 2020)

  def two(in: String): Int =
    compute(in.trim().split(',').map(_.toInt), 30000000)

  private def compute(init: Array[Int], roundLimit: Int): Int = {
    val cur = Array.fill(roundLimit)(0)
    val prev = Array.fill(roundLimit)(0)
    var round: Int = 1
    var lastNumberSpoken: Int = 0
    for(n <- init) {
      lastNumberSpoken = n
      cur(n) = round
      round += 1
    }
    while(true) {
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

      if(round == roundLimit) {
        return lastNumberSpoken
      }
      round += 1
    }
    ???
  }
}