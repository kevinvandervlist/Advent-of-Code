package nl.kevinvandervlist.aoc2020.day16

import nl.kevinvandervlist.aoc.AoCSpec

class TicketTranslationSpec extends AoCSpec {
  override def example: String =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12
      |""".stripMargin

  def example2: String =
    """departure class: 0-1 or 4-19
      |departure row: 0-5 or 8-19
      |departure seat: 0-13 or 16-19
      |
      |your ticket:
      |11,12,13
      |
      |nearby tickets:
      |3,9,18
      |15,1,5
      |5,14,9
      |""".stripMargin

  override def examplePartOne(): Any =
    TicketTranslation.one(exampleAsLines) shouldBe 71

  override def assignmentPartOne(): Any =
    TicketTranslation.one(inputAsLines) shouldBe 29759

  override def examplePartTwo(): Any =
    TicketTranslation.two(example2.split('\n').toList) shouldBe 1716

  override def assignmentPartTwo(): Any =
    TicketTranslation.two(inputAsLines) shouldBe 1307550234719L
}
