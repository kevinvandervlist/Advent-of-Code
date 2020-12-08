package nl.kevinvandervlist.aoc2020.day07

import nl.kevinvandervlist.aoc.AoCSpec

class LuggageProcessingSpec extends AoCSpec {
  override def example: String =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.
      |""".stripMargin

  def example2: String =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.
      |""".stripMargin

  override def examplePartOne(): Any =
    LuggageProcessing.one(exampleAsLines) shouldBe 4

  override def assignmentPartOne(): Any =
    LuggageProcessing.one(inputAsLines) shouldBe 112

  override def examplePartTwo(): Any = {
    LuggageProcessing.two(exampleAsLines) shouldBe 32
    LuggageProcessing.two(example2.split('\n').toList) shouldBe 126
  }

  override def assignmentPartTwo(): Any =
    LuggageProcessing.two(inputAsLines) shouldBe 6260
}
