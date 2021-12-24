package nl.kevinvandervlist.aoc2021.day23

import nl.kevinvandervlist.aoc.AoCSpec

class AmphipodSpec extends AoCSpec {
  override def example: String = ???

  private val actualExampleOne = (11, List('B', 'A'), List('C', 'D'), List('B', 'C'), List('D', 'A'))
  private val actualInputOne = (11, List('B', 'C'), List('B', 'A'), List('D', 'D'), List('A', 'C'))
  private val actualExampleTwo = (11, List('B', 'D', 'D', 'A'), List('C', 'C', 'B', 'D'), List('B', 'B', 'A', 'C'), List('D', 'A', 'C', 'A'))
  private val actualInputTwo = (11, List('B', 'D', 'D', 'C'), List('B', 'C', 'B', 'A'), List('D', 'B', 'A', 'D'), List('A', 'A', 'C', 'C'))

  override def examplePartOne(): Any =
    Amphipod.challenge(actualExampleOne) shouldBe 12521

  override def assignmentPartOne(): Any =
    Amphipod.challenge(actualInputOne) shouldBe 11608

  override def examplePartTwo(): Any =
    Amphipod.challenge(actualExampleTwo) shouldBe 44169

  override def assignmentPartTwo(): Any =
    Amphipod.challenge(actualInputTwo) shouldBe 46754
}
