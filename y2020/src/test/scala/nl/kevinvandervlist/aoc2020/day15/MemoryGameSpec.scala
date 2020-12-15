package nl.kevinvandervlist.aoc2020.day15

import nl.kevinvandervlist.aoc.AoCSpec

class MemoryGameSpec extends AoCSpec {
  override def example: String =
    """0,3,6
      |""".stripMargin

  override def examplePartOne(): Any = {
    MemoryGame.one(example) shouldBe 436
    MemoryGame.one("1,3,2") shouldBe 1
    MemoryGame.one("2,1,3") shouldBe 10
    MemoryGame.one("1,2,3") shouldBe 27
    MemoryGame.one("2,3,1") shouldBe 78
    MemoryGame.one("3,2,1") shouldBe 438
    MemoryGame.one("3,1,2") shouldBe 1836
  }

  override def assignmentPartOne(): Any =
    MemoryGame.one(inputAsString) shouldBe 517

  override def examplePartTwo(): Any = {
    MemoryGame.two(example) shouldBe 175594
//    MemoryGame.two("1,3,2") shouldBe 2578
//    MemoryGame.two("2,1,3") shouldBe 3544142
//    MemoryGame.two("1,2,3") shouldBe 261214
//    MemoryGame.two("2,3,1") shouldBe 6895259
//    MemoryGame.two("3,2,1") shouldBe 18
//    MemoryGame.two("3,1,2") shouldBe 362
  }

  override def assignmentPartTwo(): Any =
    MemoryGame.two(inputAsString) shouldBe 1047739
}
