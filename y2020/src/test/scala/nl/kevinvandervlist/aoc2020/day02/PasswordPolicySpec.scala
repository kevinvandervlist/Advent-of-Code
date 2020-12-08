package nl.kevinvandervlist.aoc2020.day02

import nl.kevinvandervlist.aoc.AoCSpec

class PasswordPolicySpec extends AoCSpec {
  override def example: String =
    """1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin

  override def examplePartOne(): Any =
    PasswordPolicy.validPasswordsOne(exampleAsLines) shouldBe 2

  override def assignmentPartOne(): Any = {
    PasswordPolicy.validPasswordsOne(inputAsLines) should not be 477
    PasswordPolicy.validPasswordsOne(inputAsLines) shouldBe 603
  }

  override def examplePartTwo(): Any =
    PasswordPolicy.validPasswordsTwo(exampleAsLines) shouldBe 1

  override def assignmentPartTwo(): Any =
    PasswordPolicy.validPasswordsTwo(inputAsLines) shouldBe 404
}
