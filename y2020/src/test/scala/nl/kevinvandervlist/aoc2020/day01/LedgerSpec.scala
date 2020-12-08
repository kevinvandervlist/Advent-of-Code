package nl.kevinvandervlist.aoc2020.day01

import nl.kevinvandervlist.aoc.AoCSpec

class LedgerSpec extends AoCSpec {
  override def example: String =
    """1721
      |979
      |366
      |299
      |675
      |1456
      |""".stripMargin

  override def examplePartOne(): Any =
    Ledger.fromStringsTwo(exampleAsLines) shouldBe BigInt(514579)

  override def assignmentPartOne(): Any =
    Ledger.fromStringsTwo(inputAsLines) shouldBe BigInt(567171)

  override def examplePartTwo(): Any =
    Ledger.fromStringsThree(exampleAsLines) shouldBe BigInt(241861950)

  override def assignmentPartTwo(): Any =
    Ledger.fromStringsThree(inputAsLines) shouldBe BigInt(212428694)
}
