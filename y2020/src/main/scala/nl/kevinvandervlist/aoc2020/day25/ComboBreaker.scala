package nl.kevinvandervlist.aoc2020.day25

object ComboBreaker {
  def one(in: List[String]): Long = {
    val pub_a = in.head.toLong
    val pub_b = in.last.toLong

    var key = 1L
    var loop = 0L
    while (key != pub_a && key != pub_b) {
      key = (key * 7) % 20201227
      loop += 1
    }

    var exp = pub_a
    if (key == pub_a) {
      exp = pub_b
    }

    key = 1
    while(loop > 0) {
      key = (key * exp) % 20201227
      loop -= 1
    }

    key
  }

  def two(in: List[String]): Long = 0
}
