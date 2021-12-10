package nl.kevinvandervlist.aoc2021.day10

import nl.kevinvandervlist.aoc.AoCSpec
import nl.kevinvandervlist.aoc2021.day10.SyntaxScoring

class SyntaxScoringSpec extends AoCSpec {
  override def example: String =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  override def examplePartOne(): Any =
    SyntaxScoring.one(exampleAsLines) shouldBe 26397

  override def assignmentPartOne(): Any =
    SyntaxScoring.one(inputAsLines) shouldBe 388713

  override def examplePartTwo(): Any =
    SyntaxScoring.two(exampleAsLines) shouldBe 288957

  override def assignmentPartTwo(): Any =
    SyntaxScoring.two(inputAsLines) shouldBe 3539961434L
}