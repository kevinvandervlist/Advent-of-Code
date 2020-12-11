package nl.kevinvandervlist.aoc2015.day04

import nl.kevinvandervlist.aoc.AoCSpec

class StockingStufferSpec extends AoCSpec {
  override def example: String = ""

  override def examplePartOne(): Any = {
    StockingStuffer.one("abcdef") shouldBe 609043
    StockingStuffer.one("pqrstuv") shouldBe 1048970
  }

  override def assignmentPartOne(): Any =
    StockingStuffer.one("iwrupvqb") shouldBe 346386

  override def examplePartTwo(): Any = {}

  override def assignmentPartTwo(): Any =
    StockingStuffer.two("iwrupvqb") shouldBe 9958218
}
