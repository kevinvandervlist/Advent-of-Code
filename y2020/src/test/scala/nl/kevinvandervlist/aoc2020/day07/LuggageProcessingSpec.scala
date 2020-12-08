package nl.kevinvandervlist.aoc2020.day07

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.io.Source

class LuggageProcessingSpec extends AnyWordSpec with Matchers {
  private val example =
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

  private val input = Source.fromResource("day-07-input").getLines().toList

  "Part 1" should {
    "validate example" in {
      LuggageProcessing.one(example.split('\n').toList) shouldBe 4
    }
    "validate assignment" in {
      LuggageProcessing.one(input) shouldBe 112
    }
  }

  "Part 2" should {
    "validate example" in {
      val example2 =
        """shiny gold bags contain 2 dark red bags.
          |dark red bags contain 2 dark orange bags.
          |dark orange bags contain 2 dark yellow bags.
          |dark yellow bags contain 2 dark green bags.
          |dark green bags contain 2 dark blue bags.
          |dark blue bags contain 2 dark violet bags.
          |dark violet bags contain no other bags.
          |""".stripMargin
      LuggageProcessing.two(example.split('\n').toList) shouldBe 32
      LuggageProcessing.two(example2.split('\n').toList) shouldBe 126
    }
    "validate assignment" in {
      LuggageProcessing.two(input) shouldBe 6260
    }
  }
}
