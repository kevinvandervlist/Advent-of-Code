package nl.kevinvandervlist.aoc2021.day21

import scala.annotation.tailrec

object DiracRace {
  def one(in: List[String]): Long = {
    val p1start = in.head.last.toString.toInt
    val p2start = in.tail.head.last.toString.toInt
    val (throwCount, p1Score, p2Score) = simulateUntilScore(1000, p1start, p2start, deterministicDice())
    List(p1Score, p2Score).min * throwCount
  }

  def two(in: List[String]): Long = {
    val p1start = in.head.last.toString.toInt
    val p2start = in.tail.head.last.toString.toInt
    val multiplier = Map[Int, Int](3 -> 1, // first scenario, all ones
      4 -> 3, // three scenarios, <2> changes over three throws
      5 -> 6, // etc etc
      6 -> 7,
      7 -> 6,
      8 -> 3,
      9 -> 1
    )
    var p1Wins = 0L
    var p2Wins = 0L

    def computeRec(p1active: Boolean, p1score: Int, p2score: Int, p1pos: Int, p2pos: Int, universes: Long): Unit = {
      for (outcome <- 3 to 9) {
        val newUniverses = universes * multiplier(outcome)
        if (p1active) {
          val newPosition = nextPlace(p1pos, outcome)
          val updatedScore = p1score + newPosition
          if(updatedScore >= 21) {
            p1Wins += newUniverses
          } else {
            computeRec(!p1active, updatedScore, p2score, newPosition, p2pos, newUniverses)
          }
        } else {
          val newPosition = nextPlace(p2pos, outcome)
          val updatedScore = p2score + newPosition
          if(updatedScore >= 21) {
            p2Wins += newUniverses
          } else {
            computeRec(!p1active, p1score, updatedScore, p1pos, newPosition, newUniverses)
          }
        }
      }
    }
    computeRec(true, 0, 0, p1start, p2start, 1)
    
    List(p1Wins, p2Wins).max
  }

  private def simulateUntilScore(atLeast: Int, p1: Int, p2: Int, dice: () => Int) = {
    var throwCount = 0L
    var p1score = 0L
    var p2score = 0L
    var p1pos = p1
    var p2pos = p2
    var p1active = true
    while(p1score < atLeast && p2score < atLeast) {
      val step = dice() + dice() + dice()
      throwCount += 3
      if(p1active) {
        p1pos = nextPlace(p1pos, step)
        p1score += p1pos
      } else {
        p2pos = nextPlace(p2pos, step)
        p2score += p2pos
      }
      p1active = ! p1active
    }
    (throwCount, p1score, p2score)
  }

  @inline
  private def nextPlace(current: Int, step: Int): Int =
    (((current - 1) + step) % 10) + 1


  private def deterministicDice(): () => Int = {
    var cnt = 1
    () => {
      val _throw = cnt
      if(cnt == 100) {
        cnt = 1
      } else {
        cnt += 1
      }
      _throw
    }
  }
}
