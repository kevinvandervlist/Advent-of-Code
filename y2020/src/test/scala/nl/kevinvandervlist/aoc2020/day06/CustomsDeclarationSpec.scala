package nl.kevinvandervlist.aoc2020.day06

import nl.kevinvandervlist.aoc.AoCSpec

class CustomsDeclarationSpec extends AoCSpec {
  override def example: String =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b
      |""".stripMargin

  override def examplePartOne(): Any =
    CustomsDeclaration.one(example) shouldBe 11

  override def assignmentPartOne(): Any =
    CustomsDeclaration.one(inputAsString) shouldBe 6768

  override def examplePartTwo(): Any =
    CustomsDeclaration.two(example) shouldBe 6

  override def assignmentPartTwo(): Any =
    CustomsDeclaration.two(inputAsString) shouldBe 3489
}
