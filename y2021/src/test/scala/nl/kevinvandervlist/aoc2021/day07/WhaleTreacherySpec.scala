package nl.kevinvandervlist.aoc2021.day07

import nl.kevinvandervlist.aoc.AoCSpec

class WhaleTreacherySpec extends AoCSpec {
  override def example: String =
    """16,1,2,0,4,2,7,1,2,14""".stripMargin

  override def examplePartOne(): Any =
    WhaleTreachery.one(example) shouldBe 37

  override def assignmentPartOne(): Any =
    WhaleTreachery.one(inputAsString) shouldBe 352707

  override def examplePartTwo(): Any =
    WhaleTreachery.two(example) shouldBe 168

  override def assignmentPartTwo(): Any =
    WhaleTreachery.two(inputAsString) shouldBe 95519693
}
